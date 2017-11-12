import axios from 'axios';
import { Notification } from 'element-ui';

const baseURL = '/yab/v1.0';

const HTTP = axios.create({
  baseURL: baseURL,
  headers: { }
})

var _response = null;

function httpWithNotify(title, eMsg, f, noSuccNotify)
{
  return f
    .then(resp => {
      if(noSuccNotify !== true)
      {
        Notification.success({
          title: title,
          duration: 5000
        });
      }

      return resp.data
    })
    .catch(e => {
      Notification.error({
        title: 'Error',
        message: eMsg + ': \n' + JSON.stringify(e.response.data),
        duration: 0
      })
    });
}

export { baseURL, HTTP, httpWithNotify }
