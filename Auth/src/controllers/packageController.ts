import { Request,Response,NextFunction } from "express-serve-static-core";
import fetch from "node-fetch";
import { Service } from "typedi";
import config from "../../config";
import IPackageController from "./IControllers/IPackageController";
const jwt = require('jsonwebtoken');

@Service()
export default class packageController implements IPackageController {

constructor() {}

private roles = ["admin","logMan"];

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

  async createPackage(req: Request, res:Response, next: NextFunction) {
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    let url = 'http://localhost:3000/api/packaging/';
    if(req.get('host').includes("azure"))
        url = 'https://logistics57.azurewebsites.net/api/packaging/';

    const data = req.body;
      const response = await fetch(url, {
        method: 'POST',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json',
          'Cookie': req.headers.cookie
        },
      })
      if(response.status != 201){
        res.status(response.status);
        return res.json({message: "Error creating Package"});
      }
      const info = await response.json();
      res.status(201);
      console.log(info);
      return res.json(info);


    };

    async getAllPackage(req: Request, res: Response, next: NextFunction) {
      if(!this.isAuthenticated(req)){
        res.status(401);
        return res.json({message: "Not authenticated"});
      }
      if(!this.isAuthorized(req)){
        res.status(403);
        return res.json({message: "Not authorized"});
      }
        let url = 'http://localhost:3000/api/packaging/all';
        if(req.get('host').includes("azure"))
          url = 'https://logistics57.azurewebsites.net/api/packaging/all/';
        const response = await fetch(url, {
          method: 'GET',
          headers: {
            'Content-Type': 'application/json',
            'Cookie': req.headers.cookie
          }
        });
        if(response.status != 200){
          res.status(response.status);
          return res.json({message: "Error getting all packages"});
        }
        const data = await response.json();
        res.status(200);
        return res.json(data);
      }



}