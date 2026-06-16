function(selectCols) {

  var api = this.api();

  // resolve column indexes from names OR indexes
  var targets = [];

  api.columns().every(function(i) {

    var header = $(this.header()).text().trim();

    if (
      selectCols.indexOf(i) !== -1 ||
      selectCols.indexOf(header) !== -1
    ) {
      targets.push(i);
    }

  });

  targets.forEach(function(i) {

    var column = api.column(i);

    var cell = $(column.header())
      .closest('table')
      .find('thead tr:eq(1) td')
      .eq(i);

    var values = column
      .data()
      .unique()
      .sort()
      .toArray()
      .filter(v => v !== null && v !== "");

    var select = $('<select multiple="multiple" style="width:100%"></select>');

    values.forEach(v => {
      select.append(`<option value="${v}">${v}</option>`);
    });

    cell.html(select);

    select.select2({
      width: "100%",
      closeOnSelect: false,
      placeholder: "All"
    });

    select.on("change", function() {

      var vals = $(this).val();

      if (!vals || vals.length === 0) {
        column.search("").draw();
        return;
      }

      var regex = vals
        .map($.fn.dataTable.util.escapeRegex)
        .join("|");

      column.search("^(" + regex + ")$", true, false).draw();

    });

  });

}