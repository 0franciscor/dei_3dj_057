import { Injectable } from '@angular/core';
import fetch from 'node-fetch';
import * as http from "http";


@Injectable({
  providedIn: 'root'
})
export class WarehouseService {
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() { }

  async getWarehouse(WarehouseId:string) {
    const url = this.urlOrigin+'api/warehouse/'+ WarehouseId;
 
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
    const url = this.urlOrigin+'api/warehouse/create';
    
    const data = warehouse;
   
    const response = fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },

    })
    return response;
          
  }

  async createWarehouseProlog(warehouse: any) {
    const url = this.urlOrigin+'api/warehouse/createProlog';
    
    const data = warehouse;
   
    const response = fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },

    })
    return response;
          
  }

  async getAllWarehouses() {
    const url = this.urlOrigin+'api/warehouse/all';

    const response = await fetch(url, {
      method: 'GET'
    })
    const data = await response.json();
    return data;
  
  }

  async updateWarehouse(warehouse: any) {

    const url = this.urlOrigin+'api/warehouse/update';
    
    const data = warehouse;

    const response = await fetch(url, {
      method: 'PUT',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    
    return response;
     
  }

  async updateWarehouseProlog(warehouse: any) {

    const url = this.urlOrigin+'api/warehouse/updateProlog';
    
    const data = warehouse;

    const response = await fetch(url, {
      method: 'PUT',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    
    return response;
     
  }


}
