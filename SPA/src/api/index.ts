import { Router } from 'express';
import roleRoute from './routes/roleRoute';
import user from './routes/userRoute';

export default () => {
	const app = Router();

	
	user(app);
	roleRoute(app)
    return app;
}