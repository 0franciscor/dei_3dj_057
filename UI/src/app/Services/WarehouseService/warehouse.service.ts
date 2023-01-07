import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class WarehouseService {
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() { }

  getJwt() {
    const cookies = document.cookie.split(';');

    let jwt = "";
    for (const cookie of cookies) {
      const [name, value] = cookie.split('=');
      if (name.trim() === "jwt") {
        jwt = value;
      }
    }
    const cookie = "jwt=" + jwt;
    return cookie;
  }

  async getAllWarehouses() {
    let url = this.urlOrigin + 'api/warehouse/all';
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/all';
    }
    const response = await this.sendFetch(url, 'GET', null, this.getJwt());
    const data = await response.json();

    return data;

  }

  async getWarehouse(WarehouseId: string) {
    let url = this.urlOrigin + 'api/warehouse/' + WarehouseId;
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/' + WarehouseId;
    }
    const response = await this.sendFetch(url, 'GET', null, this.getJwt());
    const data = await response.json();

    return data;

  }


  async createWarehouse(warehouse: any) {
    let url = this.urlOrigin + 'api/warehouse/create';
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/create';
    }
    const data = warehouse;
    const response = await this.sendFetch(url, 'POST', data, this.getJwt());

    return response;

  }


  async updateWarehouse(warehouse: any) {

    let url = this.urlOrigin + 'api/warehouse/update';
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/update';
    }
    const data = warehouse;
    const response = await this.sendFetch(url, 'PUT', data, this.getJwt());

    return response;

  }


  async activateWarehouse(warehouseId: string) {

    let url = this.urlOrigin + 'api/warehouse/active/' + warehouseId;
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/activate/' + warehouseId;
    }
    const response = await this.sendFetch(url, 'PATCH', null, this.getJwt());


    return response;

  }


  async deactivateWarehouse(warehouseId: string) {

    let url = this.urlOrigin + 'api/warehouse/deactivate/' + warehouseId;
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/deactivate/' + warehouseId;
    }
    const response = await this.sendFetch(url, 'DELETE', null, this.getJwt());

    return response;

  }


  async deleteWarehouse(warehouseId: string) {

    let url = this.urlOrigin + 'api/warehouse/delete/' + warehouseId;
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/delete/' + warehouseId;
    }
    const response = await this.sendFetch(url, 'DELETE', null, this.getJwt());

    return response;
  }


  // ############################## PROLOG RELATED METHODS ###############################


  async createWarehouseProlog(warehouse: any) {
    let url = this.urlOrigin + 'api/warehouse/createProlog';
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/createProlog';
    }

    interface createWarehouseProlog {
      id: string;
      address: string;
      altitude: number;
      latitude: string;
      longitude: string;
      designation: string;
      city: string;
    }

    const data: createWarehouseProlog = {
      id: warehouse.id,
      address: warehouse.address,
      altitude: warehouse.altitude,
      latitude: warehouse.latitude,
      longitude: warehouse.longitude,
      designation: warehouse.designation,
      city: warehouse.city
    }

    const response = await this.sendFetch(url, 'POST', data, this.getJwt());

    return response;
  }


  async updateWarehouseProlog(warehouse: any) {

    let url = this.urlOrigin + 'api/warehouse/updateProlog';
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/updateProlog';
    }

    interface createWarehouseProlog {
      id: string;
      address: string;
      altitude: number;
      latitude: string;
      longitude: string;
      designation: string;
      city: string;
    }

    const data: createWarehouseProlog = {
      id: warehouse.id,
      address: warehouse.address,
      altitude: warehouse.altitude,
      latitude: warehouse.latitude,
      longitude: warehouse.longitude,
      designation: warehouse.designation,
      city: warehouse.city
    }

    const response = await this.sendFetch(url, 'PUT', data, this.getJwt());

    return response;
  }


  async deleteWarehouseProlog(warehouseID: string) {
      
      let url = this.urlOrigin + 'api/warehouse/deleteProlog/'
      if (this.urlOrigin.includes("azure")) {
        url = 'https://auth57.azurewebsites.net/api/warehouse/deleteProlog/';
      }

      interface createWarehouseProlog {
        id: string;
      }
  
      const data: createWarehouseProlog = {
        id: warehouseID
      }
      
      const response = await this.sendFetch(url, 'DELETE', data, this.getJwt());
  
      return response;
  }

  async createCityProlog(city: any) {
    let url = this.urlOrigin + 'api/warehouse/createCityProlog';
    if (this.urlOrigin.includes("azure")) {
      url = 'https://auth57.azurewebsites.net/api/warehouse/createCityProlog';
    }

    interface createCityProlog {
      id: string;
      name: string;
    }

    const data: createCityProlog = {
      id: city.id,
      name: city.name
    };
    
    const response = await this.sendFetch(url, 'POST', data, this.getJwt());

    return response;

  }


  //###################################  SEND FETCH  #####################################

  async sendFetch(url: string, method: string, data: any, cookie: any) {
    if (data)
      //send cookie with request
      return await fetch(url, {
        method: method,
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          "authorization": cookie,


        },


      })
    else
      return await fetch(url, {
        method: method,
        headers: {
          'Accept': 'application/json',
          "authorization": cookie,
        }
      })
  }

}