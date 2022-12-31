import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IRoleDTO from '../dto/IRoleDTO';
import IRoleService from '../services/IServices/IRoleService';
import IRoleController from "./IControllers/IRoleController";

import { Result } from "../core/logic/Result";
import IUserService from '../services/IServices/IUserService';
const jwt = require('jsonwebtoken');
@Service()
export default class RoleController implements IRoleController {
  constructor(
      @Inject(config.services.role.name) private roleServiceInstance : IRoleService,
      @Inject(config.services.user.name) private userServiceInstance : IUserService
  ) {}
  
  private roles = ["admin"];

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


  public async createRole(req: Request, res: Response, next: NextFunction) {
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
    try {
      const roleOrError = await this.roleServiceInstance.createRole(req.body as IRoleDTO) as Result<IRoleDTO>;
        
      if (roleOrError.isFailure) {
        return res.status(400).send();
      }

      const roleDTO = roleOrError.getValue();
      return res.json( roleDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async updateRole(req: Request, res: Response, next: NextFunction) {
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
    try {
      const roleOrError = await this.roleServiceInstance.updateRole(req.body as IRoleDTO) as Result<IRoleDTO>;

      if (roleOrError.isFailure) {
        return res.status(404).send();
      }

      const roleDTO = roleOrError.getValue();
      return res.status(201).json( roleDTO );
    }
    catch (e) {
      return next(e);
    }
  };

  public async getRole(req: Request, res: Response, next: NextFunction){
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
    try {
      
      const role = await  this.roleServiceInstance.getRole(req.body.roleId);
      if(role.isFailure){
        res.status(404);
        return res.send("Role not found");
      }
      res.status(200)
      return res.json(role.getValue());

    } catch (error) {
      next (error)
    }
  }

  public async getAllRoles(req: Request, res: Response, next: NextFunction){
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
    try {
      const roles = await this.roleServiceInstance.getAllRoles();
      res.status(200);
      return res.json(roles.getValue());
    } catch (error) {
      next(error)
    }
  }

  public async deleteRole(req: Request, res: Response, next: NextFunction){
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
    try {
      const roleResult = await this.roleServiceInstance.deleteRole(req.body.roleId);
      if(roleResult.isFailure){
        res.status(404);
        return res.send("role not found");
      }
      res.status(200)
      return res.json(roleResult.getValue());
    } catch (error) {
      next(error)
    }
  }

  public async currentRole(req: Request, res: Response, next: NextFunction) {
    try {
      if(req.headers.authorization!=undefined)
        req.cookies["jwt"]=req.headers.authorization.split("=")[1];
      if(req.cookies['jwt'] == undefined){
        res.status(401);
        return res.send("Unauthorized");
      }
      const cookie = req.cookies['jwt'];

      const claims = jwt.verify(cookie, config.jwtSecret);
      const userOrError = await this.userServiceInstance.getUserByID(claims.id);

      if(userOrError.isFailure || userOrError.getValue().role != claims.role){
        res.status(401);
        return res.send("Unauthorized");

      }

      if(!claims){
        res.status(401);
        return res.send("Unauthorized");
      }
      res.status(200);
      return res.json(claims.role);
    } catch (error) {
      next(error)
    }
   
  }

  public async validateRole(req: Request, res: Response, next: NextFunction){
    
    try {
      if(req.headers.authorization!=undefined)
        req.cookies["jwt"]=req.headers.authorization.split("=")[1];
      if(req.cookies['jwt'] == undefined){
        res.status(401);
        return res.send("Unauthorized");
      }
      const cookie = req.cookies['jwt'];
      const claims = jwt.verify(cookie, config.jwtSecret);
      if(!claims){
        res.status(401);
        return res.send("Unauthorized");
      }
      if(claims.role == req.params.role){
        res.status(200);
        return res.send("Authorized");
      }
      res.status(403);
      return res.send("Forbidden");
     
    } catch (error) {
      next(error)
    }
  }

  

}