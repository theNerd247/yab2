import axios from 'axios';

const baseURL = '/';

const HTTP = axios.create({
  baseURL: baseURL,
  headers: {
  }
})

export { baseURL, HTTP }
