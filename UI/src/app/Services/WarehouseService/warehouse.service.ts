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
    let url = this.urlOrigin+'api/warehouse/'+ WarehouseId;
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/'+ WarehouseId;
    }
    const response = await this.sendFetch(url,'GET',null);
    const data = await response.json();

    return data;
    
  }

  async createWarehouse(warehouse: any) {
    let url = this.urlOrigin+'api/warehouse/create';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/create';
    }
    const data = warehouse;
    const response = await this.sendFetch(url, 'POST', data);

    return response;
          
  }

  async createWarehouseProlog(warehouse: any) {
    let url = this.urlOrigin+'api/warehouse/createProlog';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/createProlog';
    }
    const data = warehouse;
    const response = await this.sendFetch(url, 'POST', data);

    return response;
          
  }

  async getAllWarehouses() {
    let url = this.urlOrigin+'api/warehouse/all';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/all';
    }
    const response = await this.sendFetch(url, 'GET', null);
    const data = await response.json();

    return data;
  
  }

  async updateWarehouse(warehouse: any) {

    let url = this.urlOrigin+'api/warehouse/update';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/update';
    }
    const data = warehouse;
    const response = await this.sendFetch(url, 'PUT', data);
    
    return response;
     
  }

  async updateWarehouseProlog(warehouse: any) {

    let url = this.urlOrigin+'api/warehouse/updateProlog';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/updateProlog';
    }
    const data = warehouse;
    const response = await this.sendFetch(url, 'PUT', data);
    
    return response;
     
  }

  async activateWarehouse(warehouseId: string) {

    let url = this.urlOrigin+'api/warehouse/active/'+ warehouseId;
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/activate/'+ warehouseId;
    }
    const response = await this.sendFetch(url,'PATCH',null);
    const data = await response.json();

    return data;
     
  }

  async deactivateWarehouse(warehouseId: string) {

    let url = this.urlOrigin+'api/warehouse/deactivate/'+ warehouseId;
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/deactivate/'+ warehouseId;
    }
    const response = await this.sendFetch(url,'DELETE',null);
    const data = await response.json();

    return data;
     
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
      console.log(url);
      return await fetch(url, {
        method: method,
        headers: {
          'Accept': 'application/json'
        }
      })
  }


}
