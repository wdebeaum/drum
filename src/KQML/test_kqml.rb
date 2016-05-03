#!/usr/bin/ruby

require 'test/unit'
require_relative 'kqml'

class TestKQML < Test::Unit::TestCase
  def test_string_roundtrip
    strings = (<<-EOS).lines.collect { |l| l.strip }
      123
      123.456
      foo
      V12345
      :foo
      foo::bar
      Penn::WP$
      Penn::|:|
      "foo bar"
      "foo\\nbar"
      "foo \\"bar\\""
      (foo bar baz)
      (foo :bar baz)
      (foo bar :argle bargle :razzle (dazzle root beer))
    EOS
    strings.each { |s|
      assert_equal(s, KQML.from_s(s).to_kqml_string)
    }
  end

  def test_list_construction
    l = KQML[:foo, :bar, :baz]
    assert_kind_of(KQML, l)
    assert_equal(3, l.positional_arguments.size)
    assert_equal(0, l.keyword_arguments.size)
    assert_equal(:bar, l[1])

    l = KQML[:foo, :bar => :baz]
    assert_kind_of(KQML, l)
    assert_equal(1, l.positional_arguments.size)
    assert_equal(1, l.keyword_arguments.size)
    assert_equal(:baz, l[:bar])

    l = KQML[:foo, {:bar => :baz}, :razzle => [:dazzle, :root, :beer]]
    assert_kind_of(KQML, l)
    assert_equal(2, l.positional_arguments.size)
    assert_equal(1, l.keyword_arguments.size)
    assert_kind_of(KQML, l[1])
    assert_kind_of(KQML, l[:razzle])
  end

  def test_subscription_patterns
    assert_operator(
      KQML[:request, :"&key", :content => [:tag, :".", :"*"]],
      :===,
      KQML[:request, :sender => :CERNL, :"reply-with" => :CERNL123,
           :content => [:tag, :text => "Some text", :uttnum => 42]]
    )
    assert_operator(
      KQML.from_s('(request &key :content (annotate *))'),
      :===,
      KQML.from_s("(request :content (annotate (sequence (string-input \"This is a sentence.\\nThis is another sentence\\n\") one-sentence-per-line debug-output)))\n")
    )
  end

  def test_requests
    assert_operator(
      KQML[:request, :content => [:annotate, [:sequence, [:"string-input", "This is a sentence.\nThis is another sentence\n"], :"one-sentence-per-line", :"debug-output"]]],
      :eql?,
      KQML.from_s("(request :content (annotate (sequence (string-input \"This is a sentence.\\nThis is another sentence\\n\") one-sentence-per-line debug-output)))\n")
    )
  end
end

