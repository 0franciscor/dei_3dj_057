import { Injectable } from '@angular/core';



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

    const response = await this.sendFetch(url, 'POST', data, "");

    const jsonResponse = await response.json();

    const cookie = jsonResponse.token;
    localStorage.setItem('jwt', cookie);
    document.cookie = "jwt=" + cookie + "; path=/";
    localStorage.setItem('user', jsonResponse.userId);
    localStorage.setItem('role', jsonResponse.role);
    
    return response;
  }

  async loginWithGoogle(credentials: string) {
    // const header = new HttpHeaders().set('Content-type', 'application/json');
    // return this.httpClient.post(this.urlOrigin + "loginWithGoogle", JSON.stringify(credentials), { headers: header });
    const data = {credentials:credentials};
    const response = await this.sendFetch(this.urlOrigin + "api/user/loginWithGoogle", 'POST', data, "");
    const jsonResponse = await response.json();

    const cookie = jsonResponse.token;
    localStorage.setItem('jwt', cookie);
    document.cookie = "jwt=" + cookie + "; path=/";
  }

  async getRole() {
    let url = this.urlOrigin+'api/role/currentRole';
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/role/currentRole';
    }
    const cookies = document.cookie.split(';');
    
    let jwt = "";
    for (const cookie of cookies) {
      const [name, value] = cookie.split('=');
      if(name.trim() === "jwt"){
        jwt = value;
      }
    }
    if(jwt === ""){
      return "401";
    }
    const cookie = "jwt=" + jwt;
    const response = await this.sendFetch(url, 'GET', null, cookie );
    if(response.status == 401){
      return "401";
    }
    
    const jsonResponse = await response.json();
    return jsonResponse;
  }


  async sendFetch(url: string, method: string, data: any, cookie:string) {
    if(data)
      return await fetch(url, {
        method: method,
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          'Access-Control-Allow-Credentials': 'true',
          "authorization": cookie,
        },
      })
    else
      return await fetch(url, {
        method: method,
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json',
          'Access-Control-Allow-Credentials': 'true',
          "authorization": cookie,
        },
      })
  }
}
