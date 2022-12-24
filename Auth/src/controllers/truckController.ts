import { Request, Response, NextFunction } from 'express';
import { Service } from 'typedi';
import ITruckController from "./IControllers/ITruckController";
import fetch from 'node-fetch';
import config from '../../config';
const http = require('https');
const jwt = require('jsonwebtoken');
@Service()
export default class TruckController implements ITruckController {
  constructor() {}

  //list of authorized roles
  private roles = ["admin", "fltMan"];

  isAuthenticated(req: Request) {
    try {
      if(req.cookies['jwt'] == undefined)
        return false;
      const cookie = req.cookies['jwt'];
    
      const claims = jwt.verify(cookie, config.jwtSecret);
    
      if(!claims)
          return false;
      
      return true;
    } catch (error) {
      return false
    }
    
  }

  isAuthorized(req: Request, specifiedRoles?: string[]) {
    try {
      if(req.cookies['jwt'] == undefined)
        return false;
      const cookie = req.cookies['jwt'];
      const claims = jwt.verify(cookie, config.jwtSecret);
      if(!claims)
          return false;
      if(specifiedRoles != undefined){
          if(specifiedRoles.indexOf(claims.role) > -1)
              return true;
          return false;
      }
      else if(this.roles.indexOf(claims.role) > -1)
          return true;
      return false;
    } catch (error) {
      return false;
    }
    
}


  private async fetch(url : string, method: string, body: any, cookie:any, agent: any = null){
    
    if(body)
      return await fetch(url,{
        method : method,
        body : JSON.stringify(body),
        headers: {
          'Content-Type': 'application/json',
          'Cookie': cookie
        },
        agent: agent
      });
    else
      return await fetch(url,{
        method : method,
        headers: {
          'Content-Type': 'application/json',
          'Cookie': cookie
        },
        agent: agent
      });
  }

  async createTruck(req: Request, res: Response, next: NextFunction) {
    if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
    let url = 'http://localhost:3000/api/truck/';
    const data = req.body;
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://logistics57.azurewebsites.net/api/truck/';
    
    const response = await this.fetch(url, 'POST', data, req.headers.cookie); 
    
    if(response.status != 201){
      res.status(response.status);
      return res.json({message: "Error creating truck"});
    }
    const httpAgent = new http.Agent({rejectUnauthorized: false});
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_truck';
    const response_prolog = await this.fetch(url_prolog, 'POST', data,req.headers.cookie , httpAgent); 
    const info = await response.json();
    res.status(201);
    return res.json(info);

  }

  async createTruckProlog(req: Request, res: Response, next: NextFunction) {
    if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_truck';
    const httpAgent = new http.Agent({rejectUnauthorized: false});
    const response_prolog = await this.fetch(url_prolog, 'POST', req.body,req.headers.cookie, httpAgent);

    if(response_prolog.status != 201){
      res.status(response_prolog.status);
      return res.json({message: "Error creating truck"});
    }
    const info = await response_prolog.json();
    res.status(201);
    return res.json(info);

  }

  async getAllTruck(req: Request, res: Response, next: NextFunction) {
    
    if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];

    let url = 'http://localhost:3000/api/truck/all';

    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://logistics57.azurewebsites.net/api/truck/all';
 
    const response = await this.fetch(url, 'GET', null, req.headers.cookie);

    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error getting all trucks"});
    }
    const data = await response.json();
    res.status(200);
    return res.json(data);
  }

  async getTruck(req: Request, res: Response, next: NextFunction) {
    if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
    let url = 'http://localhost:3000/api/truck/id/'+req.body.truckId;
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://logistics57.azurewebsites.net/api/truck/id/'+req.body.truckId;
    
    const response = await this.fetch(url, 'GET', null, req.headers.cookie);
    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error getting truck"});
    }

    const data = await response.json();
    res.status(200)
    return res.json(data);
  }

  async editTruck(req: Request, res: Response, next: NextFunction) {
    if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
    let url = 'http://localhost:3000/api/truck/';
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://logistics57.azurewebsites.net/api/truck/';
      
    
    const data = req.body;
    const response = await this.fetch(url, 'PATCH', data, req.headers.cookie);
    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error editing truck"});
    }

    // const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/update_truck';
    // const httpAgent = new http.Agent({rejectUnauthorized: false});
    // const response_prolog = await this.fetch(url_prolog, 'PUT', data,req.headers.cookie, httpAgent);
    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

  async editTruckProlog(req: Request, res: Response, next: NextFunction) {
    if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];

    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/update_truck';
    const httpAgent = new http.Agent({rejectUnauthorized: false});
    const response_prolog = await this.fetch(url_prolog, 'PUT', req.body,req.headers.cookie, httpAgent);

    if(response_prolog.status != 200){
      res.status(response_prolog.status);
      return res.json({message: "Error editing truck"});
    }
    const info = await response_prolog.json();
    res.status(200);
    return res.json(info);
  }

  async softDeleteTruck(req: Request, res: Response, next: NextFunction) {
    if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
    let url = 'http://localhost:3000/api/truck/id/'+req.params.id;
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://logistics57.azurewebsites.net/api/truck/id/'+req.params.id;
    const response = await this.fetch(url, 'DELETE', null,req.headers.cookie);

    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error deleting truck"});
    }
 
    // const httpAgent = new http.Agent({rejectUnauthorized: false});
    // const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/delete_truck';
    // const response_prolog = await this.fetch(url_prolog, 'DELETE', null,req.headers.cookie, httpAgent);
    const info = await response.json();
    res.status(200);
    return res.json(info);
    
  }

  async hardDeleteTruck(req: Request, res: Response, next: NextFunction) {

    if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req,["admin"])){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
    let url = 'http://localhost:3000/api/truck/hard/id/'+req.params.id;
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://logistics57.azurewebsites.net/api/truck/hard/id/'+req.params.id;
    const response = await this.fetch(url, 'DELETE', null,req.headers.cookie);
    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error deleting truck"});
    }
 
    // const httpAgent = new http.Agent({rejectUnauthorized: false});
    // const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/delete_truck';
    // const response_prolog = await this.fetch(url_prolog, 'DELETE', null,req.headers.cookie, httpAgent);
    const info = await response.json();
    res.status(200);
    return res.json(info);
    
  }

  async deleteTruckProlog(req: Request, res: Response, next: NextFunction) {
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
  
    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/delete_truck';

    const response_prolog = await this.fetch(url_prolog, 'DELETE', null,req.headers.cookie, httpAgent);

    if(response_prolog.status != 200){
      res.status(response_prolog.status);
      return res.json({message: "Error deleting truck"});
    }

    const info = await response_prolog.json();
    res.status(200);
    return res.json(info);
    
  }

 



}