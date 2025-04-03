HTMLWidgets.widget({
  name: 'echartr',
  type: 'output',

  factory: function(el, width, height) {

    var chart;
    var initialised = false;

    return {

      renderValue: function(x) {
        if (!initialised) {
          initialised = true;
          Shiny.addCustomMessageHandler("__echartr__".concat(el.id), x => update_echartr(chart, x));
        }
        chart = echarts.init(el);
        chart.setOption(x.option);
      },

      resize: function(width, height) {
      },

    };
  }
});

function update_echartr(chart, event) {
  chart.setOption(event.option);
  console.log(chart);
  console.log(event);
}




