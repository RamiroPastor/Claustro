
import axios from 'axios';




export const APIHeaders = {
    'Accept': 'application/json',
    'Content-Type': 'application/json',
    'Access-Control-Allow-Origin': '*',
};

export const API = axios.create({
    baseURL: "/api",
    timeout: 12000,
    headers: APIHeaders
});
