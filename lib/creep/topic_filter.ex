defmodule Creep.TopicFilter do
  @moduledoc """
  Utility functions for matching topic filters
  """

  @doc "Checks if a topic_or_filter matches any filters"
  def matches_any_filters?(topic_filters, topic_or_filter)

  def matches_any_filters?([filter | rest], topic_or_filter) do
    if topic_matches_filter?(filter, topic_or_filter),
      do: true,
      else: matches_any_filters?(rest, topic_or_filter)
  end

  def matches_any_filters?([], _), do: false

  @doc "Checks if a topic is valid for a `Publish` packet"
  def topic_valid_for_publish?(topic)

  # All Topic Names and Topic Filters MUST be at least one character long [MQTT-4.7.3-1]
  def topic_valid_for_publish?(""), do: false

  def topic_valid_for_publish?("$" <> _), do: false

  # Topic Names and Topic Filters are UTF-8 encoded strings, 
  # they MUST NOT encode to more than 65535 bytes [MQTT-4.7.3-3]
  def topic_valid_for_publish?(topic) when byte_size(topic) > 65535, do: false

  def topic_valid_for_publish?(topic) when is_binary(topic) do
    topic_valid_for_publish?(split(topic))
  end

  # The multi-level wildcard character MUST be specified either on its own or 
  # following a topic level separator. In either case it MUST be the last 
  # character specified in the Topic Filter [MQTT-4.7.1-2]
  def topic_valid_for_publish?(["#" | _]), do: false

  # Topic Names and Topic Filters MUST NOT include the null 
  # character (Unicode U+0000) [Unicode] [MQTT-4.7.3-2]
  def topic_valid_for_publish?([<<0>> | _]), do: false

  def topic_valid_for_publish?([_ | rest]), do: topic_valid_for_publish?(rest)

  def topic_valid_for_publish?([]), do: true

  @doc """
  Checks if a topic matches a given filter based on the rules described
  in section 4.7 of the MQTT spec
  """
  def topic_matches_filter?(filter, topic, ignore_filters \\ false)

  def topic_matches_filter?(filter, topic, ignore_filters)
      when is_binary(filter) and is_binary(topic) do
    case split(topic) do
      # The Server MUST NOT match Topic Filters starting with a wildcard 
      # character (# or +) with Topic Names beginning with a $ character [MQTT-4.7.2-1].
      ["$" <> _ | _] = topic ->
        topic_matches_filter?(split(filter), topic, true)

      _ = topic ->
        topic_matches_filter?(split(filter), topic, ignore_filters)
    end
  end

  def topic_matches_filter?([wildcard | _filter_rest], [_ | _topic_rest], true)
      when wildcard in ["+", "#"],
      do: false

  # The single-level wildcard can be used at any level in the Topic Filter, 
  # including first and last levels. Where it is used it MUST occupy an entire 
  # level of the filter [MQTT-4.7.1-3]
  def topic_matches_filter?(["+" | filter_rest], [_ | topic_rest], false) do
    topic_matches_filter?(filter_rest, topic_rest)
  end

  # This clause is used for matching a filter to another filter
  # such as the case in UNSUBSCRIBE
  # It should not be used in the case of PUBLISH
  def topic_matches_filter?([_ | filter_rest], ["+" | topic_rest], false) do
    topic_matches_filter?(filter_rest, topic_rest)
  end

  # The multi-level wildcard character MUST be specified either on its own or 
  # following a topic level separator. In either case it MUST be the last 
  # character specified in the Topic Filter [MQTT-4.7.1-2].
  def topic_matches_filter?(["#" | _filter_rest], _, false), do: true

  # This clause is used for matching a filter to another filter
  # such as the case in UNSUBSCRIBE
  # It should not be used in the case of PUBLISH
  def topic_matches_filter?([_ | _filter_rest], ["#" | _], false), do: true

  def topic_matches_filter?([section | filter_rest], [section | topic_rest], ignore) do
    topic_matches_filter?(filter_rest, topic_rest, ignore)
  end

  def topic_matches_filter?([_filter | _], [_topic | _], _), do: false
  def topic_matches_filter?([], [], _), do: true
  def topic_matches_filter?(_, [_topic | _], _), do: false
  def topic_matches_filter?([_filter | _], _, _), do: false

  @doc "Split a topic or filter"
  def split(str, level \\ "", acc \\ [])

  def split(<<"/", rest::binary>>, level, acc) do
    split(rest, "", acc ++ [level])
  end

  def split(<<char::binary-1, rest::binary>>, level, acc) do
    split(rest, level <> char, acc)
  end

  def split(<<>>, level, acc) do
    acc ++ [level]
  end
end
