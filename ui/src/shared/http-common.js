import axios from 'axios';

export const HTTP = axios.create({
  baseURL: `http://0.0.0.0:8000/`,
  headers: {
  }
})
