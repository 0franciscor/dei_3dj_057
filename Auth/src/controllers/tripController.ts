import { Request, Response, NextFunction } from 'express';
import { Service } from 'typedi';
import ITripController from "./IControllers/ITripController";
import fetch from 'node-fetch';
import { ParamsDictionary } from 'express-serve-static-core';
import { ParsedQs } from 'qs';
const http = require('https');
@Service()
export default class TripController implements ITripController {
  constructor() {}


    async createTrip(req: Request, res: Response, next: NextFunction) {
        console.log(req.body);
        const tripURL = 'http://localhost:3000/api/trip/';
        const packagingURL = 'http://localhost:3000/api/packaging/';
        
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
          const response = await this.fetch(packagingURL, 'POST', packaging);
  
        });


        let pathIDlist: any[] = [];

        for (let index = 0; index < req.body.infoList.length-1; index++) {
          const url = 'http://localhost:3000/api/path/all/'+req.body.infoList[index].warehouse+'/'+req.body.infoList[index+1].warehouse;
          const response = await this.fetch(url, 'GET', null);
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

        const response = await this.fetch(tripURL, 'POST', trip);
        console.log(response);
       

       
    }

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


}