import { Repo } from '../../core/infra/Repo';
import { Route } from '../../domain/route/Route';
import { RouteID } from '../../domain/route/RouteID';


export default interface IRouteRepo extends Repo<Route> {
    exists(route: Route): Promise<boolean>;
    save(route: Route): Promise <Route>;
    delete(route: Route): Promise<Route>;
    getRouteById(routeID: RouteID|string): Promise <Route>;
    getAllRoutes(): Promise<Route[]>;
}