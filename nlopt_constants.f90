module nlopt_constants
    use accuracy
    implicit none

    public
    integer, parameter :: &
        NLOPT_GN_DIRECT = 0, &
        NLOPT_GN_DIRECT_L = 1, &
        NLOPT_GN_DIRECT_L_RAND = 2, &
        NLOPT_GN_DIRECT_NOSCAL = 3, &
        NLOPT_GN_DIRECT_L_NOSCAL = 4, &
        NLOPT_GN_DIRECT_L_RAND_NOSCAL = 5, &
        NLOPT_GN_ORIG_DIRECT = 6, &
        NLOPT_GN_ORIG_DIRECT_L = 7, &
        NLOPT_GD_STOGO = 8, &
        NLOPT_GD_STOGO_RAND = 9, &
        NLOPT_LD_LBFGS_NOCEDAL = 10, &
        NLOPT_LD_LBFGS = 11, &
        NLOPT_LN_PRAXIS = 12, &
        NLOPT_LD_VAR1 = 13, &
        NLOPT_LD_VAR2 = 14, &
        NLOPT_LD_TNEWTON = 15, &
        NLOPT_LD_TNEWTON_RESTART = 16, &
        NLOPT_LD_TNEWTON_PRECOND = 17, &
        NLOPT_LD_TNEWTON_PRECOND_RESTART = 18, &
        NLOPT_GN_CRS2_LM = 19, &
        NLOPT_GN_MLSL = 20, &
        NLOPT_GD_MLSL = 21, &
        NLOPT_GN_MLSL_LDS = 22, &
        NLOPT_GD_MLSL_LDS = 23, &
        NLOPT_LD_MMA = 24, &
        NLOPT_LN_COBYLA = 25, &
        NLOPT_LN_NEWUOA = 26, &
        NLOPT_LN_NEWUOA_BOUND = 27, &
        NLOPT_LN_NELDERMEAD = 28, &
        NLOPT_LN_SBPLX = 29, &
        NLOPT_LN_AUGLAG = 30, &
        NLOPT_LD_AUGLAG = 31, &
        NLOPT_LN_AUGLAG_EQ = 32, &
        NLOPT_LD_AUGLAG_EQ = 33, &
        NLOPT_LN_BOBYQA = 34, &
        NLOPT_GN_ISRES = 35, &
        NLOPT_AUGLAG = 36, &
        NLOPT_AUGLAG_EQ = 37, &
        NLOPT_G_MLSL = 38, &
        NLOPT_G_MLSL_LDS = 39, &
        NLOPT_LD_SLSQP = 40, &
        NLOPT_LD_CCSAQ = 41, &
        NLOPT_GN_ESCH = 42, &
        NLOPT_GN_AGS = 43, &
        NLOPT_FAILURE = -1, &
        NLOPT_INVALID_ARGS = -2, &
        NLOPT_OUT_OF_MEMORY = -3, &
        NLOPT_ROUNDOFF_LIMITED = -4, &
        NLOPT_FORCED_STOP = -5, &
        NLOPT_SUCCESS = 1, &
        NLOPT_STOPVAL_REACHED = 2, &
        NLOPT_FTOL_REACHED = 3, &
        NLOPT_XTOL_REACHED = 4, &
        NLOPT_MAXEVAL_REACHED = 5, &
        NLOPT_MAXTIME_REACHED = 6

contains
    function nlopt_stop_flag(ires) result(flag)
        implicit none
        integer(ik) :: ires
        character(cl):: flag

        select case(ires)
            case (NLOPT_SUCCESS)
                flag = "NLOPT_SUCCESS"
            case (NLOPT_STOPVAL_REACHED)
                flag = "NLOPT_STOPVAL_REACHED"
            case (NLOPT_FTOL_REACHED)
                flag = "NLOPT_FTOL_REACHED"
            case (NLOPT_XTOL_REACHED)
                flag = "NLOPT_XTOL_REACHED"
            case (NLOPT_MAXEVAL_REACHED)
                flag = "NLOPT_MAXEVAL_REACHED"
            case (NLOPT_MAXTIME_REACHED)
                flag = "NLOPT_MAXTIME_REACHED ="
            case (NLOPT_FAILURE)
                flag = "NLOPT_FAILURE"
            case (NLOPT_INVALID_ARGS)
                flag = "NLOPT_INVALID_ARGS"
            case (NLOPT_OUT_OF_MEMORY )
                flag = "NLOPT_OUT_OF_MEMORY "
            case (NLOPT_ROUNDOFF_LIMITED)
                flag = "NLOPT_ROUNDOFF_LIMITED"
            case (NLOPT_FORCED_STOP)
                flag = "NLOPT_FORCED_STOP"
            case default
                flag = "Unknown NLopt stop flag"
        endselect
    end function nlopt_stop_flag

    function get_nlopt_algorithm(algorithm_string) &
    result(algorithm)

        character(cl), intent(in) :: algorithm_string
        integer :: algorithm


        select case(trim(algorithm_string))            
            case default
                algorithm = NLOPT_LN_COBYLA
            case("NLOPT_GN_DIRECT")
                algorithm = NLOPT_GN_DIRECT
            case("NLOPT_GN_DIRECT_L")
                algorithm = NLOPT_GN_DIRECT_L
            case("NLOPT_GN_DIRECT_L_RAND")
                algorithm = NLOPT_GN_DIRECT_L_RAND
            case("NLOPT_GN_DIRECT_NOSCAL")
                algorithm = NLOPT_GN_DIRECT_NOSCAL
            case("NLOPT_GN_DIRECT_L_NOSCAL")
                algorithm = NLOPT_GN_DIRECT_L_NOSCAL
            case("NLOPT_GN_DIRECT_L_RAND_NOSCAL")
                algorithm = NLOPT_GN_DIRECT_L_RAND_NOSCAL
            case("NLOPT_GN_ORIG_DIRECT")
                algorithm = NLOPT_GN_ORIG_DIRECT
            case("NLOPT_GN_ORIG_DIRECT_L")
                algorithm = NLOPT_GN_ORIG_DIRECT_L
            case("NLOPT_GD_STOGO")
                algorithm = NLOPT_GD_STOGO
            case("NLOPT_GD_STOGO_RAND")
                algorithm = NLOPT_GD_STOGO_RAND
            case("NLOPT_LD_LBFGS_NOCEDAL")
                algorithm = NLOPT_LD_LBFGS_NOCEDAL
            case("NLOPT_LD_LBFGS")
                algorithm = NLOPT_LD_LBFGS
            case("NLOPT_LN_PRAXIS")
                algorithm = NLOPT_LN_PRAXIS
            case("NLOPT_LD_VAR1")
                algorithm = NLOPT_LD_VAR1
            case("NLOPT_LD_VAR2")
                algorithm = NLOPT_LD_VAR2
            case("NLOPT_LD_TNEWTON")
                algorithm = NLOPT_LD_TNEWTON
            case("NLOPT_LD_TNEWTON_RESTART")
                algorithm = NLOPT_LD_TNEWTON_RESTART
            case("NLOPT_LD_TNEWTON_PRECOND")
                algorithm = NLOPT_LD_TNEWTON_PRECOND
            case("NLOPT_LD_TNEWTON_PRECOND_RESTART")
                algorithm = NLOPT_LD_TNEWTON_PRECOND_RESTART
            case("NLOPT_GN_CRS2_LM")
                algorithm = NLOPT_GN_CRS2_LM
            case("NLOPT_GN_MLSL")
                algorithm = NLOPT_GN_MLSL
            case("NLOPT_GD_MLSL")
                algorithm = NLOPT_GD_MLSL
            case("NLOPT_GN_MLSL_LDS")
                algorithm = NLOPT_GN_MLSL_LDS
            case("NLOPT_GD_MLSL_LDS")
                algorithm = NLOPT_GD_MLSL_LDS
            case("NLOPT_LD_MMA")
                algorithm = NLOPT_LD_MMA
            case("NLOPT_LN_COBYLA")
                algorithm = NLOPT_LN_COBYLA
            case("NLOPT_LN_NEWUOA")
                algorithm = NLOPT_LN_NEWUOA
            case("NLOPT_LN_NEWUOA_BOUND")
                algorithm = NLOPT_LN_NEWUOA_BOUND
            case("NLOPT_LN_NELDERMEAD")
                algorithm = NLOPT_LN_NELDERMEAD
            case("NLOPT_LN_SBPLX")
                algorithm = NLOPT_LN_SBPLX
            case("NLOPT_LN_AUGLAG")
                algorithm = NLOPT_LN_AUGLAG
            case("NLOPT_LD_AUGLAG")
                algorithm = NLOPT_LD_AUGLAG
            case("NLOPT_LN_AUGLAG_EQ")
                algorithm = NLOPT_LN_AUGLAG_EQ
            case("NLOPT_LD_AUGLAG_EQ")
                algorithm = NLOPT_LD_AUGLAG_EQ
            case("NLOPT_LN_BOBYQA")
                algorithm = NLOPT_LN_BOBYQA
            case("NLOPT_GN_ISRES")
                algorithm = NLOPT_GN_ISRES
            case("NLOPT_AUGLAG")
                algorithm = NLOPT_AUGLAG
            case("NLOPT_AUGLAG_EQ")
                algorithm = NLOPT_AUGLAG_EQ
            case("NLOPT_G_MLSL")
                algorithm = NLOPT_G_MLSL
            case("NLOPT_G_MLSL_LDS")
                algorithm = NLOPT_G_MLSL_LDS
            case("NLOPT_LD_SLSQP")
                algorithm = NLOPT_LD_SLSQP
            case("NLOPT_LD_CCSAQ")
                algorithm = NLOPT_LD_CCSAQ
            case("NLOPT_GN_ESCH")
                algorithm = NLOPT_GN_ESCH
            case("NLOPT_GN_AGS")
                algorithm = NLOPT_GN_AGS
        end select
    end function get_nlopt_algorithm

end module nlopt_constants
