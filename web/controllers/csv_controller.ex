defmodule Accio.CsvController do
  use Accio.Web, :controller

  def export(conn, _params) do
    conn
    |> put_resp_content_type("text/csv")
    |> put_resp_header("content-disposition", "attachement; filename=\"download.csv\"")
    |> send_resp(200, csv_content)
  end

  defp csv_content do
    csv_content = [['a','list'],['of','lists']]
    |> CSV.encode
    |> Enum.to_list
    |> to_string
  end
end
