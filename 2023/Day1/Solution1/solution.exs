defmodule Solution do
  defp readFile() do
    {:ok, input} =
      File.read("./input.txt")

    input
    |> String.split("\n")
    |> Enum.to_list()
  end

  defp removeAlphaNumerics(str) do
    str
    |> String.replace(~r/[^0-9]/, "")
  end


  defp removeEmptyString(input) do
    input
    |> Enum.filter(fn str -> str != "" end)
  end

  defp toGraphemes(str) do
    str
    |> String.graphemes()
  end

  defp toNumbers(input) do
    input
    |> Enum.map(&String.to_integer/1)
  end

  defp grabHeadAndTail(input) do
    [input |> hd(), input |> Enum.reverse() |> hd()]
  end

  defp accumulateTotal(input) do
    input
    |> List.flatten()
    |> Enum.sum()
  end

  def run() do
    readFile()
    |> Enum.map(&removeAlphaNumerics/1) # Filter out characters
    |> removeEmptyString() # Remove strings without numbers
    |> Enum.map(&toGraphemes/1) # Convert strings to arrays of chars
    |> Enum.map(&grabHeadAndTail/1) # Grab first and last char of each array
    |> Enum.map(&Enum.join()/1) # Join chars back into one string
    |> toNumbers() # Convert strings to numbers
    |> accumulateTotal() # Add up the numbers
    |> IO.puts # Print to std::out
  end
end

Solution.run()
