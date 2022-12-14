import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class DeliveryService {
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() { }

  getJwt() {
    const cookies = document.cookie.split(';');
    
    let jwt = "";
    for (const cookie of cookies) {
      const [name, value] = cookie.split('=');
      if(name.trim() === "jwt"){
        jwt = value;
      }
    }
    const cookie = "jwt=" + jwt;
    return cookie;
  }

  async getDeliveries() {
    let url = this.urlOrigin + 'api/delivery/all';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/delivery/all';
    }
    const response = await this.sendFetch(url, 'GET', null, this.getJwt());

    const data = await response.json();

    return data;
  }

  async getDelivery(deliveryID: string) {
    let url = this.urlOrigin + 'api/delivery/' + deliveryID;
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/delivery/'+deliveryID;
    }
    const response = await this.sendFetch(url, 'GET', null, this.getJwt());

    const data = await response.json();

    return data;
  }

  async createDelivery(delivery: any) {
    let url = this.urlOrigin + 'api/delivery/create';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/delivery/create';
    }
    const data = delivery;

    const response = await this.sendFetch(url, 'POST', data, this.getJwt());

    return response;

  }

  async updateDelivery(delivery: any) {
    let url = this.urlOrigin + 'api/delivery/update';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/delivery/update';
    }
    const data = delivery;

    const response = await this.sendFetch(url, 'PATCH', data, this.getJwt());

    return response;
  }

  async createDeliveryProlog(delivery: any) {
    let url = this.urlOrigin + 'api/delivery/createProlog';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/delivery/createProlog';
    }
    const data = delivery;

    const response = await this.sendFetch(url, 'POST', data, this.getJwt());

    return response;
  }

  async updateDeliveryProlog(delivery: any) {
    let url = this.urlOrigin + 'api/delivery/updateProlog';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/delivery/updateProlog';
    }

    const data = delivery;

    const response = await this.sendFetch(url, 'PUT', data, this.getJwt());

    return response;
  }

  async sendFetch(url: string, method: string, data: any, cookie:any) {
    if (data)
      return await fetch(url, {
        method: method,
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json',
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
