function(){
  var id = '#%s';
  $(id).css("visibility", "hidden");
  $($($(id).find('thead')[0]).find("tr")[2]).remove();
  var tds = $($(id).find('thead')[0]).find('td');
  var ths = $(id).find('th');
  this.api().columns().every(function(i){
    var column = this;
    if($(ths[i]).text()!==''){
      var select = $('<select multiple="multiple"></select>')
        .appendTo( $(tds[i]).empty() ) 
        .on('change', function(){
          var vals = $('option:selected', this).map(function(index,element){
            return $.fn.dataTable.util.escapeRegex($(element).val());
          }).toArray().join('|');
          column.search(vals.length > 0 ? '('+vals+')' : '', true, false).draw();
        });
      var data = column.data();
      if(i === 0){
        data = data.map(function(item){
          var it = item;
          if(item != null) if(item.startsWith('<')) it = $(it).text().trim();
          return(it);
        })
        .each(function(d, j){
          select.append('<option value="'+d+'">'+d+'</option>');
        });
      }else{
        data = data.map(function(item){
          var it = item;
          if(item != null) if(item.startsWith('<')) it = $(it).text().trim();
          return(it);
        })
        .unique().sort()
        .each(function(d, j){
          select.append('<option value="'+d+'">'+d+'</option>');
        });
      }
      select.select2({width: '100%%', closeOnSelect: false});
    }else{
      $(tds[i]).empty();
    }
  });
  $(id).css("visibility", "visible");
}