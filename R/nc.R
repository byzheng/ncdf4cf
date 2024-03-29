

#' Opens an existing netCDF file for reading (or, optionally, writing).
#'
#' @param filename Name of the existing netCDF file to be opened.
#' @param write If FALSE (default), then the file is opened read-only. If TRUE, then writing to the file is allowed.
#' @return A ncdf4 object created by nc_open in ncdf4 package.
#' @export
cf_open <- function(filename, write = FALSE)
{
    ncdf4::nc_open(filename, write)
}




#' Close a nc file
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @return No return value.
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
#' @return An object of class ncdim4 that follow CF Conventions
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
    new_dim <- ncdf4::ncdim_def(name, units, vals, ...)
    new_dim$labels <- labels
    return (new_dim)
}


#' Defines a netCDF variable
#' @param name Name of the variable  to be created (character string).
#' @param units The variable's units (character string).
#' @param dim The variable's dimension(s)
#' @param ... Other arguments pass to ncvar_def
#' @return An object of class ncvar4 that follow CF Conventions
#' @export
cfvar_def <- function(name, units, dim, ...)
{
    ncvar <- ncdf4::ncvar_def(name, units, dim, ...)
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
#' @return An object of class ncdf4 that follow CF Conventions
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
        str_dim <- ncdf4::ncdim_def('str_len', '', seq(length = str_len),
                                    create_dimvar = FALSE)
        lbl_vars <- lapply(dims, function(x)
        {
            if (is.null(x$labels))
            {
                return(NULL)
            } else
            {
                lbl_var <- ncdf4::ncvar_def(sprintf('%s_lbl', x$name), '',
                                            list(str_dim, x),
                                            prec = 'char')
                return(lbl_var)
            }

        })
        vars <- append(lbl_vars[!unlist(lapply(lbl_vars, is.null))], vars)
    }
    ncnew <- ncdf4::nc_create(filename, vars, ...)
    ncdf4::nc_redef(ncnew)
    temp <- lapply(vars, function(x)
    {
        if (!is.null(x$coordinates))
        {
            ncdf4::ncatt_put(ncnew, x$name, 'coordinates',
                             x$coordinates, prec = 'text', definemode = TRUE)
        }
    })
    ncdf4::ncatt_put(ncnew, 0, 'Conventions',
                     'CF-1.6', prec = 'text',
                     definemode = TRUE)
    ncdf4::nc_enddef(ncnew)

    temp <- lapply(dims, function(x)
    {
        if (!is.null(x$labels))
        {
            ncdf4::ncvar_put(ncnew, sprintf('%s_lbl', x$name),
                             x$labels)
        }
    })
    return (ncnew)
}


#' Get all variable names from nc file
#'
#' @param nc An object of class ncdf4 (as returned from ncOpen), indicating what file to read from.
#' @return A vector of variable names in the NetCDF file.
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
#' @return A vector of dimension names.
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


# Get all labels for dimension
#
# @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
# @param name The dimesion name
cfvar_lbl <- function(nc, name)
{
    nc_var <- nc$var[[name]]
    if (is.null(nc_var))
    {
        var_labels <- seq(length = nc$dim[[name]]$len)
    } else
    {
        var_labels <- ncdf4::ncvar_get(nc, name)
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
#' @param dimension Dimension of input variables
#' @return A vector of all labels for a dimension.
#' @export
cfvar_lables <- function(nc, name, dimension = NULL)
{
    if (is.null(nc$var[[name]]))
    {
        stop(sprintf('%s doesn\'t exist', name))
    }
    coordinates <- ncdf4::ncatt_get(nc, name, attname = 'coordinates')
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

#' Get variables values from nc file
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating what file to read from.
#' @param varname The variables names in the nc file.
#' @param ... Arguments are used to specify which data will be returned.
#' @return A named array for data stored in the NetCDF file.
#' @export
cfvar_get <- function(nc, varname, ...)
{
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
        values <- ncdf4::ncvar_get(nc, varname)
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
        values <- ncdf4::ncvar_get(nc, varid = varname)
    } else
    {
        dim_value <- as.numeric(unlist(lapply(f_dimnames, length)))
        values <- array(rep(NA, prod(dim_value)), dim = dim_value)
        for (i in seq(nrow(f_start_g)))
        {
            m_value <- ncdf4::ncvar_get(nc, varid = varname,
                                        start = as.numeric(f_start_g[i,]), count = as.numeric(f_len_g[i,]))

            args_subset <- list(values)
            for (j in seq(length = ncol(f_start_g)))
            {
                args_subset[[j+1]] <- seq(idx_start_g[i,j], idx_end_g[i,j])
            }
            args_subset[[j+2]] <- m_value
            values <- do.call(`[<-`, args_subset)
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
#' @param prec Accuracy of dimension
#' @return No return values
#' @export
cfarr_nc <- function(..., filename, prec = 'float')
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
        cfvar_def(x, '', dim_nc, compression = 9, prec = prec))
    nc <- cf_create(filename, var_nc)
    for (i in seq(along = variables))
    {
        ncdf4::ncvar_put(nc, names(variables)[i], variables[[i]])
    }
    ncdf4::nc_close(nc)
}
