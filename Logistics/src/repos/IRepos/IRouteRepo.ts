import { Repo } from '../../core/infra/Repo';
import { Route } from '../../domain/route/Route';
import { RouteID } from '../../domain/route/RouteID';
import { TruckPlate } from '../../domain/route/TruckPlate';

export default interface IRouteRepo extends Repo<Route> {
    exists(route: Route): Promise<boolean>;
    save(route: Route): Promise <Route>;
    delete(route: Route): Promise<Route>;
    getRouteByPlate(truckPlate: TruckPlate|string): Promise <Route>;
    getAllRoutes(): Promise<Route[]>;
}