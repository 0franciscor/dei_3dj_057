import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IPathController from "./IControllers/IPathController";

import { Result } from "../core/logic/Result";
import fetch from 'node-fetch';

const http = require('https');
const jwt = require('jsonwebtoken');
@Service()
export default class PathController implements IPathController {
  constructor() {}

  private roles = ["admin"];

  isAuthenticated(req: Request) {
    if(req.cookies['jwt'] == undefined)
      return false;
    const cookie = req.cookies['jwt'];
    const claims = jwt.verify(cookie, config.jwtSecret);
    if(!claims)
        return false;
    
    return true;
  }

  isAuthorized(req: Request) {
    if(req.cookies['jwt'] == undefined)
      return false;
    const cookie = req.cookies['jwt'];
    const claims = jwt.verify(cookie, config.jwtSecret);
    if(!claims)
        return false;
    if(claims.role in this.roles)
      return false;
    return true;
  }

  public async getAllPaths(req: Request, res: Response, next: NextFunction){
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    let address = 'http://localhost:3000/api/path/all/'+req.params.startWHId+'/'+req.params.destinationWHId;
    
    if(req.get('host').includes("azure"))
      address = 'https://logistics57.azurewebsites.net/api/path/all/'+req.params.startWHId+'/'+req.params.destinationWHId;
    
    const response = await fetch(address, {
        method: 'GET',
    });
    
    if (response.status == 404){
        res.status(404)
        return res.send("No paths found");   
    }else{
        let data = await response.json();
        res.status(200)
        return res.json(data);
    }
  }

  async createPath(req:Request,res:Response,next:NextFunction){
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    let url = 'http://localhost:3000/api/path/'
    if(req.get('host').includes("azure"))
      url = 'https://logistics57.azurewebsites.net/api/path/'
    const data = req.body;
    const response = await fetch(url,{
      method: 'POST',
      body:JSON.stringify(data),
      headers:{
        'Content-Type': 'application/json'
      },
    })

    if(response.status != 201){
      res.status(response.status);
      return res.json({message: "Error creating path"});
    }
    const info = await response.json();
    res.status(201);
    return res.json(info);
  }

  async createPathProlog(req:Request,res:Response,next:NextFunction){
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_path';

    const response_prolog = await fetch(url_prolog, {
      method: 'POST',
      body: JSON.stringify(req.body),
      headers: {
        'Content-Type': 'application/json'
      },
      agent: httpAgent
    })

    if(response_prolog.status != 201){
      res.status(response_prolog.status);
      return res.json({message: "Error creating truck"});
    }
    const info = await response_prolog.json();
    res.status(201);
    return res.json(info);
  }
}