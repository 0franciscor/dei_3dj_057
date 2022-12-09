import { Injectable } from '@angular/core';
import { EMPTY } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class LoginService {
  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() { }

  async login(loginInfo: any) {
    let url = this.urlOrigin+'api/user/login/';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/user/login/';
    }
    
    const data = loginInfo;
    const response = await this.sendFetch(url, 'POST', data);

    const jsonResponse = await response.json();

    const cookie = jsonResponse.token;
    localStorage.setItem('jwt', cookie);
    document.cookie = "jwt=" + cookie + "; path=/";
    localStorage.setItem('user', jsonResponse.userId);
    localStorage.setItem('role', jsonResponse.role);
    
    return response;
  }

  logout() {
    console.log(document.cookie)
    document.cookie = "jwt= ; expires = Thu, 01 Jan 1970 00:00:00 GMT"
    console.log(document.cookie)
    localStorage.clear();
  }


  async getRole() {
    let url = this.urlOrigin+'api/role/currentRole';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/role/currentRole';

    }
    const response = await this.sendFetch(url, 'GET', null);
    if(response.status == 401){
      return "401";
    }
    const jsonResponse = await response.json();
    return jsonResponse;
  }


  async sendFetch(url: string, method: string, data: any) {
    if(data)
      return await fetch(url, {
        method: method,
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          'Access-Control-Allow-Credentials': 'true',
          "authorization": document.cookie,
        },
      })
    else
      return await fetch(url, {
        method: method,
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json',
          'Access-Control-Allow-Credentials': 'true',
          "authorization": document.cookie,
        }
      })
  }
}
