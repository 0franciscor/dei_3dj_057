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

  async createPackage(req: Request, res:Response, next: NextFunction) {
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
    let url = 'http://localhost:3000/api/packaging/';
    if(req.get('host').includes("azure"))
        url = 'https://logistics57.azurewebsites.net/api/packaging/';

    const data = req.body;
    const response = await this.fetch(url, 'POST', data, req.headers.cookie);
    // const response = await fetch(url, {
    //   method: 'POST',
    //   body: JSON.stringify(data),
    //   headers: {
    //     'Content-Type': 'application/json',
    //     'Cookie': req.headers.cookie
    //   },
    // })
    
    if(response.status != 201){
      res.status(response.status);
      return res.json({message: "Error creating Package"});
    }
    const info = await response.json();
    res.status(201);
    
    return res.json(info);


  };

  async getAllPackage(req: Request, res: Response, next: NextFunction) {
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
      let url = 'http://localhost:3000/api/packaging/all';
      if(req.get('host').includes("azure"))
        url = 'https://logistics57.azurewebsites.net/api/packaging/all/';
      const response = await this.fetch(url, 'GET', null, req.headers.cookie);
      // const response = await fetch(url, {
      //   method: 'GET',
      //   headers: {
      //     'Content-Type': 'application/json',
      //     'Cookie': req.headers.cookie
      //   }
      // });
      if(response.status != 200){
        res.status(response.status);
        return res.json({message: "Error getting all packages"});
      }
      const data = await response.json();
      res.status(200);
      return res.json(data);
  }



}