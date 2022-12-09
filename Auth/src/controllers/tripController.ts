import { Request, Response, NextFunction } from 'express';
import { Service } from 'typedi';
import ITripController from "./IControllers/ITripController";
import fetch from 'node-fetch';
import { ParamsDictionary } from 'express-serve-static-core';
import { ParsedQs } from 'qs';
import config from '../../config';
const http = require('https');
const jwt = require('jsonwebtoken');
@Service()
export default class TripController implements ITripController {
  constructor() {}

  private roles = ["admin", "logMan"];

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
    if(this.roles.indexOf(claims.role) > -1)
      return true;
    return false;
  }

    async createTrip(req: Request, res: Response, next: NextFunction) {
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
        let tripURL = 'http://localhost:3000/api/trip/';
        let packagingURL = 'http://localhost:3000/api/packaging/';
        if(req.get('host').includes("azure")){
          tripURL = 'https://logistics57.azurewebsites.net/api/packaging/';
          packagingURL = 'https://logistics57.azurewebsites.net/api/packaging/';
        }
        const packagingID = req.body.infoList[0].delivery + req.body.truckName;
        req.body.infoList.forEach(async info => {
          const deliveryID = info.delivery;
          const x = 1;
          const y = 1;
          const z = 1;
          const truckName = req.body.truckName;
          const packagingID = deliveryID + truckName;
          
          const packaging = {
            packagingID: packagingID,
            truckID: truckName,
            deliveryID: deliveryID,
            xPosition: x,
            yPosition: y,
            zPosition: z
          }
          const response = await this.fetch(packagingURL, 'POST', packaging, req.headers.cookie);
  
        });


        let pathIDlist: any[] = [];

        for (let index = 0; index < req.body.infoList.length-1; index++) {
          let url = 'http://localhost:3000/api/path/all/'+req.body.infoList[index].warehouse+'/'+req.body.infoList[index+1].warehouse;
          if(req.get('host').includes("azure"))
            url= 'https://logistics57.azurewebsites.net/api/path/all/'+req.body.infoList[index].warehouse+'/'+req.body.infoList[index+1].warehouse;
          const response = await this.fetch(url, 'GET', null, req.headers.cookie);
          const jsonResponse = await response.json();
          pathIDlist.push(jsonResponse[0].pathID);
        }

        const trip = {
          tripID: req.body.planDate+req.body.truckName,
          date: req.body.planDate,
          pathIDlist: pathIDlist,
          truckID: req.body.truckName,
          packagingID: packagingID
        }

        const response = await this.fetch(tripURL, 'POST', trip, req.headers.cookie);
       
       

       
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


}