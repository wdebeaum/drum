# Wrapper to make the jdbc-sqlite3 JRuby gem look like the sqlite3-ruby MRI gem.
# (at least to the WordNetSQL library)

require 'java'
require 'jdbc/sqlite3'
Jdbc::SQLite3.load_driver
Java::OrgSqlite::JDBC

module SQLite3
  class ResultSet
    def initialize(java)
      @java = java
      @num_columns = @java.get_meta_data.get_column_count
      @rows = []
      @eof = false
    end

    def eof?
      @eof
    end

    def next
      return nil if (@eof)
      @eof = (not @java.next)
      return nil if (@eof)
      @rows <<
	(1..@num_columns).collect { |col_index| @java.get_object(col_index) }
      @rows[-1]
    end

    def close
      @java.close
    end

    # iterates over *remaining* rows, not all rows (because this is what sqlite3-ruby does)
    def each
      while (row = self.next)
	yield row
      end
    end

    include Enumerable

    def [](i)
      unless (i < @rows.size)
        each { |row|
	  break if (i < @rows.size)
	}
      end
      @rows[i]
    end
  end

  class Statement
    def initialize(java)
      @java = java
    end

    def execute(*bind_vars)
      bind_vars.flatten.each_with_index { |var, index|
        @java.set_object(index+1, var)
      }
      result_set = ResultSet.new(@java.execute_query)
      yield result_set if (block_given?)
      result_set
    end

    def close
      @java.close
    end
  end

  class Database
    def initialize(filename)
      @java = Java::JavaSql::DriverManager.get_connection('jdbc:sqlite:' + filename)
    end

    def results_as_hash=(val)
      raise "results_as_hash=true unsupported by this wrapper" if (val)
    end

    def type_translation=(val)
      raise "type_translation=false unsupported by this wrapper" unless (val)
    end

    def prepare(sql)
      stmt = Statement.new(@java.prepare_statement(sql))
      return stmt unless (block_given?)
      begin
        yield stmt
      ensure
        stmt.close
      end
    end

    def execute(sql, bind_vars=[], &block)
      result_set = prepare(sql).execute(*bind_vars)
      begin
	if (block_given?)
	  result_set.each(&block)
	else
	  result_set.to_a
	end
      ensure
	result_set.close
      end
    end

    def query(sql, bind_vars=[])
      result_set = prepare(sql).execute(*bind_vars)
      if (block_given?)
	begin
	  yield result_set
	ensure
	  result_set.close
	end
      else
	result_set
      end
    end

    # taken unchanged from sqlite3-ruby
    def self.quote( string )
      string.gsub( /'/, "''" )
    end
  end
end
