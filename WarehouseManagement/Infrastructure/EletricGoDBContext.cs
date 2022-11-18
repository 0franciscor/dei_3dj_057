using EletricGo.Domain.Cities;
using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Warehouses;
using EletricGo.Infrastructure.Cities;
using EletricGo.Infrastructure.Deliveries;
using EletricGo.Infrastructure.Warehouses;
using Microsoft.EntityFrameworkCore;

namespace EletricGo.Infrastructure
{
    public class EletricGoDBContext : DbContext
    {
        public DbSet<Delivery> Delivery { get; set; }
        
        public DbSet<Warehouse> Warehouse { get; set; }
        
        public DbSet<City> City { get; set; }

        public EletricGoDBContext(DbContextOptions options) : base(options)
        {
        }


        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new DeliveryEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new WarehouseEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new CityEntityTypeConfiguration());
        }
    }
    
}