F90=gfortran

# if you use plthm, please adapt the path of your PGPLOT
# library in the line below
PGPLOT = -L/usr/local/pgplot -lpgplot -lX11
# for the PGPLOT software package, look at
# http://www.astro.caltech.edu/~tjp/pgplot/

OBJDIR=obj
LIBDIR=lib
OBJ_FOR_LIB=zibconst.o linalg_alcon1.o alcon1.o 
OBJ_DEST=$(addprefix $(OBJDIR)/,$(OBJ_FOR_LIB))

%.o : %.f
	@mkdir -p obj
	@echo "Compiling $*.f ..."
	@$(F90) -ffixed-form -c $*.f -o $(OBJDIR)/$*.o 

libalcon.a : $(OBJ_FOR_LIB)
	@mkdir -p lib
	@$(AR) rcs $(LIBDIR)/$@ $(OBJ_DEST)
	@chmod 755 $(LIBDIR)/$@
	@ranlib $(LIBDIR)/$@

all: libalcon.a

clean:
	@-rm -f *~
	@-rm -f *.o
	@-rm -f $(OBJDIR)/*.o
	@-rm -f $(LIBDIR)/libgc.a
