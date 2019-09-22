defmodule Creep.TopicFilterTest do
  use ExUnit.Case, async: true
  alias Creep.TopicFilter

  test "matches_any_filters?/2" do
    assert TopicFilter.matches_any_filters?(["a", "b", "a/b"], "a")
    assert TopicFilter.matches_any_filters?(["a", "b", "a/b"], "b")
    assert TopicFilter.matches_any_filters?(["a", "b", "a/b"], "a/b")
    refute TopicFilter.matches_any_filters?(["a", "b", "a/b"], "a/c")

    assert TopicFilter.matches_any_filters?(["a/b"], "a/#")
    assert TopicFilter.matches_any_filters?(["a/#"], "a/#")
  end

  test "normal operation" do
    assert TopicFilter.topic_matches_filter?("a", "a")
    assert TopicFilter.topic_matches_filter?("a space character", "a space character")
    assert TopicFilter.topic_matches_filter?("/", "/")

    assert TopicFilter.topic_matches_filter?("a/b", "a/b")
    refute TopicFilter.topic_matches_filter?("a/b", "a/c")
    refute TopicFilter.topic_matches_filter?("a/b", "a/c/d")
    refute TopicFilter.topic_matches_filter?("a", "a/c/d")
    refute TopicFilter.topic_matches_filter?("a/b/c/d", "a")
    refute TopicFilter.topic_matches_filter?("/finance", "finance")

    assert TopicFilter.topic_valid_for_publish?("a")
    assert TopicFilter.topic_valid_for_publish?("a/b")
    assert TopicFilter.topic_valid_for_publish?("/")

    refute TopicFilter.topic_valid_for_publish?(:binary.copy("a", 65536))
  end

  test "multi-level wildcard (#)" do
    filter = "myhome/groundfloor/#"
    assert TopicFilter.topic_matches_filter?(filter, "myhome/groundfloor/livingroom/temperature")
    assert TopicFilter.topic_matches_filter?(filter, "myhome/groundfloor/kitchen/temperature")
    assert TopicFilter.topic_matches_filter?(filter, "myhome/groundfloor/kitchen/brightness")
    assert TopicFilter.topic_matches_filter?(filter, "myhome/groundfloor")
    refute TopicFilter.topic_matches_filter?(filter, "myhome")
    refute TopicFilter.topic_matches_filter?(filter, "myhome/firstfloor/kitchen/brightness")
  end

  test "single-level wildcard (+)" do
    filter = "myhome/groundfloor/+/temperature"
    assert TopicFilter.topic_matches_filter?(filter, "myhome/groundfloor/livingroom/temperature")
    assert TopicFilter.topic_matches_filter?(filter, "myhome/groundfloor/kitchen/temperature")
    refute TopicFilter.topic_matches_filter?(filter, "myhome/groundfloor/kitchen/brightness")
    refute TopicFilter.topic_matches_filter?(filter, "myhome/firstfloor/kitchen/temperature")
    refute TopicFilter.topic_matches_filter?(filter, "myhome/groundfloor")
    refute TopicFilter.topic_matches_filter?(filter, "myhome")

    refute TopicFilter.topic_matches_filter?(
             filter,
             "myhome/groundfloor/kitchen/fridge/temperature"
           )

    assert TopicFilter.topic_matches_filter?("+", "myhome")
    refute TopicFilter.topic_matches_filter?("+", "myhome/groundfloor")
    refute TopicFilter.topic_matches_filter?("sport/+", "sport")
    assert TopicFilter.topic_matches_filter?("sport/+", "sport/")
    assert TopicFilter.topic_matches_filter?("+/tennis/#", "sport/tennis/abc")
    refute TopicFilter.topic_matches_filter?("sport+", "sport/tennis")
    assert TopicFilter.topic_matches_filter?("+/+", "/finance")
    assert TopicFilter.topic_matches_filter?("+/+", "/+")
    refute TopicFilter.topic_matches_filter?("+/+", "+")
  end

  test "$ system character" do
    assert TopicFilter.topic_matches_filter?("$SYS/broker", "$SYS/broker")
    assert TopicFilter.topic_matches_filter?("$SYS/broker/1", "$SYS/broker/1")
    refute TopicFilter.topic_matches_filter?("$SYS/+/1", "$SYS/broker/1")
    assert TopicFilter.topic_matches_filter?("$/1", "$/1")
    refute TopicFilter.topic_matches_filter?("$/#", "$/1")
    refute TopicFilter.topic_matches_filter?("#", "$SYS")

    refute TopicFilter.topic_valid_for_publish?("$SYS")
    refute TopicFilter.topic_valid_for_publish?("$")
    refute TopicFilter.topic_valid_for_publish?("$/SYS")
  end
end
