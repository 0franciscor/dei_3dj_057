import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IPathController from "./IControllers/IPathController";

import { Result } from "../core/logic/Result";
import fetch from 'node-fetch';

const http = require('https');

@Service()
export default class PathController implements IPathController {
  constructor() {}

 

  public async getAllPaths(req: Request, res: Response, next: NextFunction){
    
    const address = 'http://localhost:3000/api/path/all/'+req.params.startWHId+'/'+req.params.destinationWHId;
    
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
    const url = 'http://localhost:3000/api/path/'
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