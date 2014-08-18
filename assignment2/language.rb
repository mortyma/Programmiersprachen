require_relative 'task'
require_relative 'context'

class Program
  attr_reader :procedures

  def initialize(procs)
    @procedures={}
    procs.each do |p|
      @procedures[p.procname.name] = p
    end
    resolve_procnames
  end

  # Create a TaskQueue and enqueue specified procedure call
  # @return [TaskQueue]
  def queue_for_call(procname, *args, &block)
    task = procedures[procname].new_task(nil, *args, &block)
    TaskQueue.new([task])
  end

  # Run specified procedure call
  # @return results from procedure
  def run(procname, *args)
    queue = queue_for_call(procname, *args) do |ret|
      queue.kill
      return ret
    end
    queue.run
  end

  private
  def resolve_procnames
    @procedures.values.each do |p|
      p.commands.each do |gcmd|
        cmd = gcmd.command
        if cmd.respond_to? :procname
          procname = cmd.procname.name
          raise "Procedure not found: #{procname} " unless @procedures.key? procname
          cmd.procedure = @procedures[procname]
        end
      end
    end
  end


end

class Procedure
  attr_reader :procname, :commands, :params, :results

  def initialize(procname, params, results, commands)
    @procname=procname
    @params=params
    @results=results
    @commands=commands
  end

  def finished?(ctx)
    ctx.all_bound?(@results)
  end

  def ready_commands(ctx)
    @commands.select{|cmd| cmd.ready?(ctx)}
  end

  # create a Task corresponding to the procedure call
  # @param *args [*String] arguments to the procedure
  # @param &finish_block [Block] block to yield results to
  # @return [Task] that enqueues all ready commands, and itself if there is any work left
  def new_task(parent_ctx, *actual_params, &finish_block)
    ctx = Context.new(@params,actual_params,parent_ctx)
    scheduled = []
    this_task = Task.new do |task_queue|
      if not ctx.alive?
        # exit
      elsif finished?(ctx)
        actual_results = ctx.get_multiple(@results)
        ctx.kill
        finish_block.call(actual_results)
      else
        ready = ready_commands(ctx)
        if ready.empty?
          raise "Procedure stuck #{self} #{ctx}" if ctx.bound?(Identifier.finally)
          ctx.set(Identifier.finally,'')
          ready = ready_commands(ctx)
        end

        to_schedule = ready - scheduled
        to_schedule.each do |cmd|
          scheduled.push(cmd)
          task_queue.push(cmd.new_task(ctx))
        end

        task_queue.push(this_task)
      end
    end
    this_task
  end

  def to_s
    "<proc:#{@procname.name}>"
  end
end

class GCommand < Struct.new(:guards, :command)
  # check that all guards are true and command ready
  def ready?(ctx)
    guards.all?{|g| g.true?(ctx)} and command.ready?(ctx)
  end

  # @return new [Task] to run command if it is (still) ready
  def new_task(ctx)
    Task.new do |task_queue|
      puts self
      #  by single-assignment true guards cannot become false
      raise "Guard error (single-assignment violation?) in #{self}" unless guards.all?{|g| g.true?(ctx)}
      # don't run if task no longer ready (because )
      if command.ready?(ctx) and ctx.alive?
        command.run(task_queue, ctx)
      end
    end
  end

  def to_s
    "guarded#{command}"
  end
end

class Guard
  # @param rvs [Array] formal parameters
  # @param condition [block] closure to be called with actual parameters
  def initialize(rvs, &condition)
    @condition = condition || proc {true}
    @rvs = rvs
  end

  # check that all parameters are bound and condition evaluates to true
  def true?(ctx)
    ctx.all_bound?(@rvs) and @condition.call(*ctx.get_multiple(@rvs))
  end
end

class Command
  def initialize(lvs=[],rvs=[])
    @lvs = lvs
    @rvs = rvs
  end

  #  check that all parameters are bound and all results are unbound
  def ready?(ctx)
    ctx.all_bound?(@rvs) and @lvs.all?{|x| x.nil? or not ctx.bound?(x)}
  end

  def body
    raise "abstract method not implemented"
  end

  # run body with @rvs and set @lvs to return values
  def run(task_queue, ctx)
    actual_params = ctx.get_multiple(@rvs)
    ret = body(*actual_params)
    ctx.set_multiple(@lvs, ret)
  end
end

class BlockCommand < Command
  def initialize(lvs=[], rvs=[], &block)
    super(lvs, rvs)
    @block = block
  end

  def body(*params)
    @block.call(*params)
  end

  def to_s
    "<block:#{@block}>"
  end
end

class ProcCommand < Command
  attr_reader :procname

  def initialize(results, procname, params)
    super(results, params)
    @procname=procname
  end

  def procedure=(proc)
    raise "Invalid number of arguments #{proc}" unless proc.params.size == @rvs.size
    raise "Invalid number of results #{proc}" unless proc.results.size == @lvs.size
    @procedure=proc
  end

  def run(task_queue, ctx)
    actual_params = ctx.get_multiple(@rvs)
    task = @procedure.new_task(ctx, *actual_params) do |actual_results|
      ctx.set_multiple(@lvs,actual_results)
    end

    task_queue.push(task)
  end

  def to_s
    "<call:#{@procedure}>"
  end

end


class MapCommand < Command
  attr_reader :procname

  def initialize(results, procname, delim, params)
    super(results, params)
    @procname=procname
    @delim=delim
  end

  def procedure=(proc)
    raise "Invalid number of arguments #{proc}" unless proc.params.size == @rvs.size
    raise "Invalid number of results #{proc}" unless proc.results.size == @lvs.size
    @procedure=proc
  end

  def ready?(ctx)
    ctx.bound?(@delim) and super
  end

  def run(task_queue, ctx)
    actual_params = ctx.get_multiple(@rvs)
    delim = ctx.get(@delim)
    actual_params.map! {|x| x.split(delim) }

    length =  actual_params[0].size
    raise "Unequal length of lists #{actual_params} in #{self}" unless actual_params.all?{ |x| x.size == length }
    results = Array.new(length)

    1.upto(length).zip(*actual_params).each do |i, *params|
      task = @procedure.new_task(ctx, *params) do |actual_results|
        results[i-1]=actual_results
      end
      task_queue.push(task)
    end

    task = Task.new do |task_queue|
      if ctx.alive?
        if results.all?{|x| not x.nil?}
          results = results.transpose
          results.map! {|x| x.join(delim) }
          ctx.set_multiple(@lvs,results)
        else
          task_queue.push(task)
        end
      end
    end
    task_queue.push(task)

  end

  def to_s
    "<map:#{procname}>"
  end

end
