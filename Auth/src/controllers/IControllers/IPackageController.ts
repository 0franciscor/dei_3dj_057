import { Request, Response, NextFunction } from 'express';

export default interface IPackageController  {
createPackage(req: Request, res: Response, next: NextFunction);
getAllPackage(req: Request, res: Response, next: NextFunction);
  
}