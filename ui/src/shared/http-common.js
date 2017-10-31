import axios from 'axios';

const baseURL = '/yab/v1.0';

const HTTP = axios.create({
  baseURL: baseURL,
  headers: {
  }
})

export { baseURL, HTTP }
