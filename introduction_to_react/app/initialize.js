import ReactDOM from 'react-dom';
import React from 'react';
import ReactDOMServer from 'react-dom/server';

class MyApp extends React.Component {

  constructor(props) {
    super(props)
    this.state = { count : 1 }
  }

  componentDidMount() {
    var that = this
    setInterval(function() {
      console.log(that.state.count);
      var b = parseInt(that.state.count) + 1
      that.setState({count: b});
      console.log(that.state.count);
    }, 100);
  }

  render() {
    var that = this
    return(
      <h1 className="header">{that.state.count}</h1>
    )
  }
}
var h1 = React.createElement(MyApp);

ReactDOM.render(h1, document.getElementById('app'));
