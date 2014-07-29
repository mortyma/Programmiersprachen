# an piece of work that can be executed concurrently
class Task
	# @param task [block] code to be executed
	def initialize(&task)
		@proc=Proc.new(&task)
	end

	def call(*args)
		@proc.call(*args)
	end
end

# queue that can be called from multiple threads
class TaskQueue
	def initialize(tasks=[])
		@queue=Queue.new
		tasks.each{|task| @queue.push(task)}
	end

	def runNext
		task = @queue.shift
		task.call(self)
	end

	def run
		while not @queue.empty?
			runNext
		end
	end

	def push(task)
		@queue.push(task)
	end
end