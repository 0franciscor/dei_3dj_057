import { Request, Response, NextFunction } from 'express';

export default interface ITruckController  {
  
  createTruck(req: Request, res: Response, next: NextFunction)
  createTruckProlog(req: Request, res: Response, next: NextFunction)
  getAllTruck(req: Request, res: Response, next: NextFunction)
  getTruck(req: Request, res: Response, next: NextFunction)
  editTruck(req: Request, res: Response, next: NextFunction)
  deleteTruck(req: Request, res: Response, next: NextFunction)
  editTruckProlog(req: Request, res: Response, next: NextFunction)
  deleteTruckProlog(req: Request, res: Response, next: NextFunction)
  
}