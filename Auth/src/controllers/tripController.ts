import { NextFunction, Request, Response } from 'express';
import fetch from 'node-fetch';
import { Service } from 'typedi';
import config from '../../config';
import ITripController from "./IControllers/ITripController";
const http = require('https');
const jwt = require('jsonwebtoken');
@Service()
export default class TripController implements ITripController {
  constructor() {}

  private roles = ["admin", "logMan"];

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
        if(req.get('host').includes("azure")){
          tripURL = 'https://logistics57.azurewebsites.net/api/packaging/';
        }
      


        let pathIDlist: any[] = [];
        let deliveryIDlist: any[]=[];

        

        for (let index = 0; index < req.body.infoList.length-1; index++) {
          let url = 'http://localhost:3000/api/path/all/'+req.body.infoList[index].warehouse+'/'+req.body.infoList[index+1].warehouse;
          if(req.get('host').includes("azure"))
            url= 'https://logistics57.azurewebsites.net/api/path/all/'+req.body.infoList[index].warehouse+'/'+req.body.infoList[index+1].warehouse;
          const response = await this.fetch(url, 'GET', null, req.headers.cookie);
          const jsonResponse = await response.json();
          pathIDlist.push(jsonResponse[0].pathID);
        }

        

        req.body.infoList.forEach(info=> {
          deliveryIDlist.push(info.delivery)
        });

        

        const trip = {
          tripID: req.body.planDate+req.body.truckName,
          date: req.body.planDate,
          pathIDlist: pathIDlist,
          truckID: req.body.truckName,
          deliveryIDlist: deliveryIDlist
        }

        const response = await this.fetch(tripURL, 'POST', trip, req.headers.cookie);
       
       

       
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