# an piece of work that can be executed concurrently
class Task
	# @param task [block] code to be executed
	def initialize(&task)
		@proc=Proc.new(&task)
	end

	def call(*args)
		# puts '--------------start:'+@proc.to_s
		@proc.call(*args)
		# puts '--------------stop:'+@proc.to_s

	end
end

# queue that can be called from multiple threads
class TaskQueue
	def initialize(tasks=[])
		@queue = tasks
		@mutex = Mutex.new
		@waiting = ConditionVariable.new
		@alive = true
	end

	class QueueEmpty < StandardError
	end

	def run
		while @alive
			begin
				task = pop(1)
				task.call(self)
			rescue QueueEmpty
				# ignore
			end
		end
	end

	def kill
		@mutex.synchronize { @alive = false }
	end

	def push(task)
		@mutex.synchronize do
			@queue.push(task)
			@waiting.signal
		end
	end

	def pop(timeout = nil)
		@mutex.synchronize do
			if @queue.empty?
				@waiting.wait(@mutex, timeout) if timeout != 0
				raise QueueEmpty if @queue.empty?
			end
			@queue.shift
		end
	end
end