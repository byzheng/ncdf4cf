# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   9:05 PM Tuesday, 29 January 2013
# * Copyright: AS IS
# *
# * $Revision: 4305 $
# * $Id: nc.R 4305 2014-08-21 15:11:16Z zhe00a $
# * $Author: zhe00a $
# * $Date: 2014-08-22 01:11:16 +1000 (Fri, 22 Aug 2014) $


#' @docType package
#' ...
#' @import ncdf4
ncdf4cf <- function()
{
}


#' Opens an existing netCDF file for reading (or, optionally, writing).
#'
#' @param filename Name of the existing netCDF file to be opened.
#' @param If FALSE (default), then the file is opened read-only. If TRUE, then writing to the file is allowed.
#' @export
cf_open <- function(filename, write = FALSE)
{
    library(ncdf4)
    ncdf4::nc_open(filename, write)
}

#' Close a nc file
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @export
cf_close <- function(nc)
{
    ncdf4::nc_close(nc)
}
#' Defines a netCDF dimension
#' @param name Name of the dimension to be created (character string).
#' @param units The dimension's units (character string).
#' @param vals The dimension's values (vector of numeric type or string).
#' If the type is string, a new dimension "str_len" and variable "name_ldl" 
#' will be created. The "coordinates" attribures will be added to relativly variables
#' @param ... Other arguments pass to ncdim_def
#' @export
cfdim_def <- function(name, units, vals, ...)
{
    if (is.factor(vals))
    {
        vals <- as.character(vals)
    }
    labels <- NULL
    if (mode(vals) == 'character')
    {
        labels <- vals
        vals <- seq(along = vals)
    }
    new_dim <- ncdim_def(name, units, vals, ...)
    new_dim$labels <- labels
    return (new_dim)
}


#' Defines a netCDF variable
#' @param name Name of the variable  to be created (character string).
#' @param units The variable's units (character string).
#' @param dim The variable's dimension(s)
#' @param ... Other arguments pass to ncvar_def
#' @export
cfvar_def <- function(name, units, dim, ...)
{
    ncvar <- ncvar_def(name, units, dim, ...)
    coor <- unlist(lapply(ncvar$dim, function(x) 
        {
            if (is.null(x$labels))
            {
                return(NULL)
            } else
            {
                return (x$name)
            }
        }))
    coor <- paste(sprintf('%s_lbl', rev(coor)), collapse = ' ')
    if (nchar(coor) > 0)
    {
        ncvar$coordinates <- coor
    }
    return(ncvar)
}

#' Creates a new netCDF file on disk, given the variables the new file is to contain. 
#'
#' The "name_lbl" varibles and "coordinates" attribures will be created according the type of dimension.
#' The global attribure "Conventions" will be created.
#' @param filename Name of the netCDF file to be created.
#' @param vars     Either an object of class ncvar4 describing the variable to be created, or a vector (or list) of such objects to be created.
#' @param ... Other arguments pass to nc_create
#' @export
cf_create <- function(filename, vars, ...)
{
    dims <- unique(unlist(lapply(vars, function(x) x$dim), recursive = FALSE))
    str_len <- max(unlist(lapply(dims, function(x)
        {
            if (is.null(x$labels))
            {
                return (0)
            } else
            {
                return (max(nchar(x$labels)))
            }
        })))
    if (str_len > 0)
    {
        str_dim <- ncdim_def('str_len', '', seq(length = str_len),
            create_dimvar = FALSE)
        lbl_vars <- lapply(dims, function(x)
            {
                if (is.null(x$labels))
                {
                    return(NULL)
                } else
                {
                    lbl_var <- ncvar_def(sprintf('%s_lbl', x$name), '',    
                        list(str_dim, x),
                        prec = 'char')
                    return(lbl_var)
                }
                
            })
        vars <- append(lbl_vars[!unlist(lapply(lbl_vars, is.null))], vars)
    }
    ncnew <- nc_create(filename, vars, ...)
    nc_redef(ncnew)
    temp <- lapply(vars, function(x)
        {
            if (!is.null(x$coordinates))
            {
                ncatt_put(ncnew, x$name, 'coordinates', 
                    x$coordinates, prec = 'text', definemode = TRUE)
            }
        })    
    ncatt_put(ncnew, 0, 'Conventions', 
            'CF-1.6', prec = 'text',
            definemode = TRUE)
    nc_enddef(ncnew)
    
    temp <- lapply(dims, function(x)
        {
            if (!is.null(x$labels))
            {
                ncvar_put(ncnew, sprintf('%s_lbl', x$name), 
                    x$labels)
            }
        })    
    return (ncnew)
}


#' Get all variable names from nc file
#'
#' @param nc An object of class ncdf4 (as returned from ncOpen), indicating what file to read from.
#' @export
cfvar_names <- function(nc)
{
    vars <- nc$var
    vars_name <- as.character(unlist(lapply(vars, function(x) x$name)))
    pos <- grep('_lbl', vars_name)
    if (length(pos > 0))
    {
        vars_name <- vars_name[-pos]
    }
    return(vars_name)
}

#' Get all dimension names for a variable
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @param var The variable name
#' @export
cfdim_names <- function(nc, var)
{
    vars <- nc$var[[var]]
    dim_name <- NULL
    for (i in seq(length = vars$ndims))
    {
        dim_name <- c(dim_name, vars$dim[[i]]$name)
    }
    return(dim_name)
}


#' Get all labels for dimension
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @param name The dimesion name
cfvar_lbl <- function(nc, name)
{
    nc_var <- nc$var[[name]]
    if (is.null(nc_var))
    {
        var_labels <- seq(length = nc$dim[[name]]$len)
    } else
    {
        var_labels <- ncvar_get(nc, name)
        pos <- nchar(var_labels) == 0
        if (sum(pos) > 0)
        {
            var_labels[pos] <- seq(along = var_labels)[pos]
        }
    }
    return(var_labels)
}


#' Get labels for all dimensions
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @param name The variable name
#' @export
cfvar_lables <- function(nc, name, dimension = NULL)
{
    if (is.null(nc$var[[name]]))
    {
        stop(sprintf('%s doesn\'t exist', name))
    }
    coordinates <- ncatt_get(nc, name, attname = 'coordinates')
    if (coordinates$hasatt)
    {
        coordinates <- strsplit(coordinates$value, '+ ')[[1]]
    } else
    {
        coordinates <- NULL
    }
    res <- NULL
    dims <- nc$var[[name]]$dim
    for (i in seq(along = dims))
    {
        dim_name <- dims[[i]]$name
        if (is.null(dimension) || dim_name %in% dimension)
        {
            coor_var <- NA
            for (j in seq(along = coordinates))
            {
                if (dim_name %in% unlist(lapply(nc$var[[coordinates[j]]]$dim, 
                    function(x) x$name)))
                {
                    coor_var <- coordinates[j]
                    break
                }
            }
            if (is.na(coor_var))
            {
                if (is.null(nc$dim[[dim_name]]$vals))
                {
                    nc_levels <- seq(length = nc$dim[[dim_name]]$len)
                } else
                {
                    nc_levels <-  nc$dim[[dim_name]]$vals
                }
            } else
            {
                nc_levels <- cfvar_lbl(nc, coor_var)
            }
            res[[dims[[i]]$name]] <- nc_levels
        }
    }
    return(res)
}

#' Get varibles values from nc file
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @param varname The variables names in the nc file.
#' @param ... Arguments are used to specify which data will be returned.
#' @export
cfvar_get <- function(nc, varname, ...)
{
    library(ncdf4)
    factors <- list(...)
    if (length(varname) != 1)
    {
        stop('Only one varname support.')
    }
    if (!varname %in% cfvar_names(nc))
    {
        stop(paste(varname, ' does not exist.', sep = ''))
    }
    
    if (nc$var[[varname]]$prec == 'char')
    {
        values <- ncvar_get(nc, varname)
        return (values)
    }
    missing_var <- nc$var[[varname]]$missval
    
    
    factors_name <- names(factors)
    f_start <- as.list(NULL)
    f_len <- as.list(NULL)
    f_dimnames <- as.list(NULL)
    
    splitVector <- function(x, xmin = 1, xmax = 9999999999999999999999999)
    {
        x <- sort(unique(x))
        pos <- x >= xmin & x <= xmax
        if (!all(pos, TRUE))
        {
            warning('Dimensions is out of range. Omit them.')
        }
        x <- x[pos]
        all_x <- seq(min(x), max(x))
        if (length(x) == length(all_x))
        {
            res <- list(NULL)
            res[[1]] <- x
            return(res)
        }
        diff_x <- all_x[!(all_x %in% x)]
        diff_x <- c(min(x) - 1, diff_x[c(TRUE, 
            diff_x[-1] - diff_x[-length(diff_x)] > 1)], max(x) + 1)
        all_parts <- as.list(NULL)
        for (i in seq(length = length(diff_x) - 1))
        {
            all_parts[[i]] <- x[x > diff_x[i] & x < diff_x[i + 1]]
        }
        return(all_parts)
    }
    
    
    
    for (i in seq(length = nc$var[[varname]]$ndims))
    {
        c_dim <- nc$var[[varname]]$dim[[i]]
        dim_name <- c_dim$name
        
        nc_levels <- cfvar_lables(nc, varname, dim_name)[[1]]
        if (dim_name %in% factors_name)
        {
            dim_levels <- factors[[dim_name]]
            
            # Check if the dimension levels with minus symbols
            # minus_dims <- grep('^-', dim_levels)
            # if (length(minus_dims) > 0 & length(minus_dims) != length(dim_levels))
            # {
                # stop('All dimensions must have "-" symbols')
            # }
            # if (length(minus_dims) > 0)
            # {
                # dim_levels <- nc_levels[!(nc_levels %in% gsub('^-', '', dim_levels))]
            # }
                        
            pos <- match(dim_levels, nc_levels)
            
            if (sum(is.na(pos)) > 0)
            {
                stop(sprintf('Levels "%s" don\'t exist in the dimension "%s"',
                    paste(dim_levels, collapse = ', '), dim_name))
            }
            pos <- splitVector(pos, xmax = c_dim$len)
            f_start[[dim_name]] <- as.numeric(lapply(pos, FUN = function(x) x[1]))
            f_len[[dim_name]] <- as.numeric(lapply(pos, FUN = length))
            nc_levels <- nc_levels[nc_levels %in% dim_levels]
        } else
        {
            f_start[[dim_name]] <- 1
            f_len[[dim_name]] <- c_dim$len
        }
        f_dimnames[[dim_name]] <- as.character(nc_levels)
    }
    
    
    f_start_g <- expand.grid(f_start)
    f_len_g <- expand.grid(f_len)
    idx_start <- lapply(f_len, function(x) c(1, cumsum(x[-length(x)]) + 1)) 
    idx_start_g <- expand.grid(idx_start)
    idx_end <- lapply(f_len, cumsum) 
    idx_end_g <- expand.grid(idx_end)
    if (is.null(factors))
    {
        values <- ncvar_get(nc, varid = varname)
    } else
    {
        dim_value <- as.numeric(unlist(lapply(f_dimnames, length)))
        values <- array(rep(NA, prod(dim_value)), dim = dim_value)
        for (i in seq(nrow(f_start_g)))
        {
            m_value <- ncvar_get(nc, varid = varname, 
                start = as.numeric(f_start_g[i,]), count = as.numeric(f_len_g[i,]))
            
            m_idx <- as.matrix(expand.grid(lapply(seq(1, length(f_start)), function(j)
                {
                    seq(idx_start_g[i,j], idx_end_g[i,j])
                })))
            values[m_idx] <- as.numeric(m_value)
        }
    }
    dimnames(values) <- f_dimnames
    return(values)
}

#' Convert an array into a new netCDF file on disk, given the variables the new file is to contain. 
#'
#' An array will be convert into a new netCDF with CF convention
#' @param ... variables write into netCDF file
#' @param filename Name of the netCDF file to be created.
#' @export
cfarr_nc <- function(..., filename)
{
    variables <- list(...)
    dims <- dimnames(variables[[1]])
    dim_names <- names(dims)
    dim_nc <- list()
    for (i in seq(along = dims))
    {
        dim_nc[[i]] <- cfdim_def(dim_names[i], '', dims[[i]])
    }
    var_nc <- lapply(names(variables), function(x) 
        cfvar_def(x, '', dim_nc, compression = 9))
    nc <- cf_create(filename, var_nc)
    for (i in seq(along = variables))
    {
        ncvar_put(nc, names(variables)[i], variables[[i]])
    }
    nc_close(nc)
}
