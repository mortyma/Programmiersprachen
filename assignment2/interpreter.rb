#!/usr/bin/env ruby
require_relative 'parser'

program = parse( ARGF.read )
# puts program.run('BEGIN')


q = program.queue_for_call('BEGIN') { |ret| 
  puts ret
  q.kill
}

workers = 1.upto(6).map { Thread.new { q.run } }
workers.each{|t| t.join }
