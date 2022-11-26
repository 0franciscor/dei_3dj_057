import { Request, Response, NextFunction } from 'express';

export default interface IDeliveryController  {
  createDelivery(req: Request, res: Response, next: NextFunction);
  getAllDeliveries(req: Request, res: Response, next: NextFunction);
  updateDelivery(req: Request, res: Response, next: NextFunction);
}