HTMLWidgets.widget({
  name: 'echartr',
  type: 'output',

  factory: function(el, width, height) {

    var chart;

    return {

      renderValue: function(x) {
        chart = echarts.init(el);
        chart.setOption(x.opt);
      },

      resize: function(width, height) {
      }

    };
  }
});
