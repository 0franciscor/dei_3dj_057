import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class PackagingService {

  constructor() { }
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  
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

  async getPackage() {
    let url = this.urlOrigin+'api/packaging/all';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/packaging/all';
    }

    const response = await this.sendFetch(url, 'GET', null, this.getJwt());

    const data = await response.json();
    return data
  }

  async createPackage(packageL:any){
    let url= this.urlOrigin+'api/packaging/'
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/packaging/';
    }
    const data = packageL;
    const response = await this.sendFetch(url, 'POST', data, this.getJwt());

    return response;
  }

  async sendFetch(url: string, method: string, data: any, cookie: any) {
    if(data)
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