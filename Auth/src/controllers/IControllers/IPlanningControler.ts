import { Request, Response, NextFunction } from 'express';

export default interface IPlanningController  {
    findAllBestPath(req: Request, res: Response, next: NextFunction)
    heuristicMass(req: Request, res: Response, next: NextFunction)
    heuristicClosestWarehouse(req: Request, res: Response, next: NextFunction)
    heuristicMassAndDistance(req: Request, res: Response, next: NextFunction)
    geneticAlgorithm(req: Request, res: Response, next: NextFunction)
}