import { Injectable } from '@angular/core';
import fetch from 'node-fetch';
import * as http from "http";

@Injectable({
  providedIn: 'root'
})
export class WarehouseService {

  constructor() { }

  async createWarehouse(warehouse: any) {
    const url = 'https://localhost:5001/api/warehouses';
    //const http = require('https')
    const data = warehouse;
    //const httpAgent = new http.Agent({ rejectUnauthorized: false });
    fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json',
        "rejectUnauthorized":"false"
      },
      //agent: httpAgent
    })
  }

}
