using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;
using EletricGo.Infrastructure;
using EletricGo.Infrastructure.Deliveries;
using EletricGo.Infrastructure.Shared;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Pomelo.EntityFrameworkCore.MySql.Infrastructure;
using System;
using EletricGo.Domain.Cities;
using EletricGo.Domain.Warehouses;
using EletricGo.Infrastructure.Cities;
using EletricGo.Infrastructure.Warehouses;
using EletricGo.Services;

namespace EletricGo
{
    public class Startup
    {
        static String MyAllowSpecificOrigins = "_myAllowSpecificOrigins";
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            

            services.AddCors(options =>
            {
                options.AddPolicy(name: MyAllowSpecificOrigins,
                    builder =>
                    {
                        builder.WithOrigins("http://localhost:3000", "http://localhost:4200");
                    });
            });

            services.AddDbContext<EletricGoDBContext>(opt =>
                        opt.UseMySql(Configuration.GetConnectionString("Default"),
                        new MySqlServerVersion(new Version(10, 7, 3)),
                        o => o.SchemaBehavior(MySqlSchemaBehavior.Ignore))

                    .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>()
            ); ;

            var optionsBuilder = new DbContextOptionsBuilder<EletricGoDBContext>();
            optionsBuilder.UseMySql(Configuration.GetConnectionString("Default"),
                        new MySqlServerVersion(new Version(10, 7, 3)),
                        o => o.SchemaBehavior(MySqlSchemaBehavior.Ignore));


            using (var dbContext = new EletricGoDBContext(optionsBuilder.Options))
            {
                dbContext.Database.EnsureCreated();
            }


            ConfigureMyServices(services);

            //initialization of the dbContext


            services
                .AddControllers()
                .AddNewtonsoftJson();

            
                

        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }
            else
            {
                // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
                app.UseHsts();
            }

            app.UseHttpsRedirection();

            app.UseRouting();
            app.UseCors(MyAllowSpecificOrigins);

            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });
        }

        public void ConfigureMyServices(IServiceCollection services)
        { 
            services.AddTransient<IUnitOfWork, UnitOfWork>();

            services.AddTransient<IDeliveryRepository, DeliveryRepository>();
            services.AddTransient<DeliveryService>();
            
            
            services.AddTransient<IWarehouseRepository, WarehouseRepository>();
            services.AddTransient<WarehouseService>();
            
            services.AddTransient<ICityRepository, CityRepository>();
            services.AddTransient<CityService>();

        }

    }
}
