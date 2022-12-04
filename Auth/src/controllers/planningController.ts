import { Request, Response, NextFunction } from 'express';
import { Service } from 'typedi';
import fetch from 'node-fetch';
import IPlanningController from './IControllers/IPlanningControler';
const http = require('https');

@Service()
export default class PlanningController implements IPlanningController {
  constructor() {}

  private async fetch(url : string, method: string, body: any, agent: any = null){
   
    if(body)
      return await fetch(url,{
        method : method,
        body : JSON.stringify(body),
        headers: {
          'Content-Type': 'application/json'
        },
        agent: agent
      });
    else
      return await fetch(url,{
        method : method,
        headers: {
          'Content-Type': 'application/json'
        },
        agent: agent
      });
  }


  async findAllBestPath(req: Request, res: Response, next: NextFunction) {
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/bestPath_findAll';
    const httpAgent = new http.Agent({rejectUnauthorized: false});

    const data = req.body;
    const response = await this.fetch(url_prolog, 'POST', data, httpAgent); 

    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

  async heuristicMass(req: Request, res: Response, next: NextFunction) {
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/heuristic_mass';
    const httpAgent = new http.Agent({rejectUnauthorized: false});

    const data = req.body;
    const response = await this.fetch(url_prolog, 'POST', data, httpAgent); 

    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

  async heuristicClosestWarehouse(req: Request, res: Response, next: NextFunction) {
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/heuristic_closestWarehouse';
    const httpAgent = new http.Agent({rejectUnauthorized: false});

    const data = req.body;
    const response = await this.fetch(url_prolog, 'POST', data, httpAgent); 

    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

  async heuristicMassAndDistance(req: Request, res: Response, next: NextFunction) {
    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/heuristic_massAndDistance';
    const httpAgent = new http.Agent({rejectUnauthorized: false});

    const data = req.body;
    const response = await this.fetch(url_prolog, 'POST', data, httpAgent); 

    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

}



