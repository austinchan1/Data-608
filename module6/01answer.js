d3.csv('ue_industry.csv', data => {

   console.log(data);

   //create scales

   const xScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.index))
        .range([1180, 20]);
    
    const yScale = d3.scaleLinear()
        .domain(d3.extent(data, d => +d.Agriculture))
        .range([580, 20]);

    //specify line

    let line = d3.line()
    	.x(d => xScale(d.index))
    	.y(d => yScale(d.Agriculture))

    //select statement for line

    d3.select('#answer1')
        .append('path')
        .attr('d', line(data))
        .attr('stroke', '#2e2928')


});
