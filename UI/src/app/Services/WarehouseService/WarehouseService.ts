import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class WarehouseService {

  constructor() { }

  async createWarehouse(warehouse: any) {
    const url = 'http://localhost:5000/api/warehouses/';
    const data = warehouse;
    // const httpAgent = new http.Agent({ rejectUnauthorized: false });
    fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
      // agent: httpAgent
    })
  }

}
