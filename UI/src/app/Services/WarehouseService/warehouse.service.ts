import { Injectable } from '@angular/core';
import fetch from 'node-fetch';
import * as http from "http";


@Injectable({
  providedIn: 'root'
})
export class WarehouseService {

  constructor() { }

  async getWarehouse(WarehouseId:string) {
    console.log(WarehouseId);
    const url = 'http://localhost:3001/api/warehouse/'+ WarehouseId;
    
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });

    const data = await response.json();

    return data;
  }

  async createWarehouse(warehouse: any) {
    const url = 'http://localhost:3001/api/warehouse/create';
    const data = warehouse;
    
    let answer;

    const response = fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },

    })
    console.log(response);
    return response;

    
  }

  async getAllWarehouses() {
    let test: any[] = [];
    const url = 'http://localhost:3001/api/warehouse/all';
    const response = await fetch(url, {
      method: 'GET'
    })
    const data = await response.json();
    return data;
    
  }

  async updateWarehouse(warehouse: any) {

    const url = 'http://localhost:3001/api/warehouse/update';
    const data = warehouse;

    console.log(data);

    const response = await fetch(url, {
      method: 'PUT',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    
    return response;

  }

  async deleteWarehouse(WarehousId: string) {
    const url = 'http://localhost:3001/api/warehouse/delete/'+WarehousId;
    
    const response = await fetch(url, {
      method: 'DELETE',
      headers: {
        'Accept': 'application/json'
      }
    });
    

    return response;
  }



}
