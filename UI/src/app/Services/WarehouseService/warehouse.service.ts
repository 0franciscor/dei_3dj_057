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
 
    const response = await this.sendFetch(url,'GET',null);
    const data = await response.json();

    return data;
    
  }

  async createWarehouse(warehouse: any) {
    const url = this.urlOrigin+'api/warehouse/create';
    
    const data = warehouse;
    const response = await this.sendFetch(url, 'POST', data);

    return response;
          
  }

  async createWarehouseProlog(warehouse: any) {
    const url = this.urlOrigin+'api/warehouse/createProlog';
    
    const data = warehouse;
    const response = await this.sendFetch(url, 'POST', data);

    return response;
          
  }

  async getAllWarehouses() {
    const url = this.urlOrigin+'api/warehouse/all';

    const response = await this.sendFetch(url, 'GET', null);
    const data = await response.json();

    return data;
  
  }

  async updateWarehouse(warehouse: any) {

    const url = this.urlOrigin+'api/warehouse/update';
    
    const data = warehouse;
    const response = await this.sendFetch(url, 'PUT', data);
    
    return response;
     
  }

  async updateWarehouseProlog(warehouse: any) {

    const url = this.urlOrigin+'api/warehouse/updateProlog';
    
    const data = warehouse;
    const response = await this.sendFetch(url, 'PUT', data);
    
    return response;
     
  }

  async sendFetch(url: string, method: string, data: any) {
    if(data)
      return await fetch(url, {
        method: method,
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
      })
    else
      return await fetch(url, {
        method: method,
        headers: {
          'Accept': 'application/json'
        }
      })
  }


}
