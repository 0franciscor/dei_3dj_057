import { NextFunction, Request, Response } from 'express';
import { Service } from 'typedi';
import config from "../../config";

import IPathController from "./IControllers/IPathController";

import fetch from 'node-fetch';

const http = require('https');
const jwt = require('jsonwebtoken');
@Service()
export default class PathController implements IPathController {
  constructor() {}

  private roles = ["admin","logMan"];

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

  public async getAllPaths(req: Request, res: Response, next: NextFunction){
    if(req.headers.origin != undefined){
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
    }
    let address = 'http://localhost:3000/api/path/all/'+req.params.startWHId+'/'+req.params.destinationWHId;
    
    if(req.get('host').includes("azure"))
      address = 'https://logistics57.azurewebsites.net/api/path/all/'+req.params.startWHId+'/'+req.params.destinationWHId;
    
    const response = await this.fetch(address, 'GET', null, req.headers.cookie);
    // const response = await fetch(address, {
    //     method: 'GET',
    //     headers: {
    //       'Content-Type': 'application/json',
    //       'Cookie': req.headers.cookie
    //     },
    // });
    
    if (response.status != 200){
        res.status(response.status)
        return res.send(response.json());   
    }else{
        let data = await response.json();
        res.status(200)
        return res.json(data);
    }
  }

  async createPath(req:Request,res:Response,next:NextFunction){
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
    let url = 'http://localhost:3000/api/path/'
    if(req.get('host').includes("azure"))
      url = 'https://logistics57.azurewebsites.net/api/path/'
    const data = req.body;
    const response = await this.fetch(url, 'POST', data, req.headers.cookie);
    // const response = await fetch(url,{
    //   method: 'POST',
    //   body:JSON.stringify(data),
    //   headers: {
    //     'Content-Type': 'application/json',
    //     'Cookie': req.headers.cookie
    //   },
    // })

    if(response.status != 201){
      res.status(response.status);
      return res.json({message: "Error creating path"});
    }
    const info = await response.json();
    res.status(201);
    return res.json(info);
  }

  async createPathProlog(req:Request,res:Response,next:NextFunction){
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
    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_path';

    const response_prolog = await this.fetch(url_prolog, 'POST', req.body, req.headers.cookie, httpAgent);
    // const response_prolog = await fetch(url_prolog, {
    //   method: 'POST',
    //   body: JSON.stringify(req.body),
    //   headers: {
    //     'Content-Type': 'application/json',
    //     'Cookie': req.headers.cookie
    //   },
    //   agent: httpAgent
    // })

    if(response_prolog.status != 201){
      res.status(response_prolog.status);
      return res.json({message: "Error creating truck"});
    }
    const info = await response_prolog.json();
    res.status(201);
    return res.json(info);
  }

  private async fetch(url : string, method: string, body: any, cookie:any, agent: any = null){
    try {
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
    } catch (error) {
      return {status: 503, json(): any{ return {message: "Error connecting to server"}}};
    }
    
  }
}