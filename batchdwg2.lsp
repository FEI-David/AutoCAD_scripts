;; BatchDwg2.lsp
;; To Batch Drawings
;;
;; Note: replace the Path/filename below to call up your Lisp
;;       to process the drawings the way you want.
;;

(setq my_lisp_file "C:/Users/david/OneDrive - franyie/Documents/AutoCAD Sheet Sets/P50-PSLTS.LSP"); Place your lisp here !

;; Load Supporting Functions
;; Old Version of 'BrowseForFolder' by: Tony Tanzillo 
(defun BrowseForFolder ( Message / sh folder parentfolder folderobject result) 
 (vl-load-com) 
  (setq sh 
   (vla-getInterfaceObject 
     (vlax-get-acad-object) 
       "Shell.Application" 
     ) 
   ) 


   (setq folder 
      (vlax-invoke-method 
          sh 
          'BrowseForFolder 
          0 
          Message 
          0 
       ) 
   ) 
   (vlax-release-object sh) 


    (if folder 
      (progn 
         (setq parentfolder 
           (vlax-get-property folder 'ParentFolder) 
         ) 
        (setq FolderObject 
           (vlax-invoke-method 
              ParentFolder 
               'ParseName 
              (vlax-get-property Folder 'Title) 
           ) 
        ) 
       (setq result 
          (vlax-get-property FolderObject 'Path) 
       ) 
       (mapcar 'vlax-release-object 
         (list folder parentfolder folderobject) 
       ) 
     (if (/= (substr result (strlen result)) "\\")
       (setq result (strcat result "\\"))
       result
     )
   ) 
 ) 
); defun

(defun C:BATCH-DWG ()
;; portions by Tim Willey
  (if (setq DirPath (BrowseForFolder "Select directory to batch drawings."))
   (progn
    (setq DwgList (vl-directory-files DirPath "*.dwg" 1))
    (setq ScrFile (strcat DirPath "batchme.scr"))
    (setq Ofil (open ScrFile "W"))
     (write-line "SDI 0" Ofil); Force Multi-Document mode
     (write-line (strcat "(setvar " (chr 34) "FILEDIA" (chr 34) " 0)") Ofil)
     (foreach Dwg DwgList
      (setq FullPath (strcat DirPath Dwg))
      (write-line (strcat "_.open " (chr 34) FullPath (chr 34)) Ofil)
      (write-line (strcat "(load " (chr 34) my_lisp_file (chr 34) ")") Ofil)
      (write-line "_.qsave" Ofil)
      (write-line "_.close" Ofil)
     ); foreach
     (write-line (strcat "(setvar " (chr 34) "FILEDIA" (chr 34) " 1)") Ofil)
     (close Ofil)
     (command "_.script" ScrFile)
   ); progn
  ); if
 (princ)
); function
 (prompt "\nType BATCH-DWG to begin the batch process:")
 (princ)
