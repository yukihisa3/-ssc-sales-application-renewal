****************************************************************
*    顧客名　　　　　：　_サカタのタネ殿　　　　              *
*    業務名　　　　　：　基幹システム　　                      *
*    モジュール名　　：　オペレータ　コール                    *
*    作成日／更新日　：　08/09/01                              *
*    作成者／更新者　：　ＮＡＶ　高橋　　　　　　　　　　　　　*
*    処理概要説明　　：　　　　　　　　　　　　　　　　　　　　*
*    使用ＭＥＤ      ：  ＦＰＡＵＳＥ　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION                  DIVISION.
 PROGRAM-ID.                     PAUSE.
 ENVIRONMENT                     DIVISION.
 CONFIGURATION                   SECTION.
 SOURCE-COMPUTER.                FACOM.
 OBJECT-COMPUTER.                FACOM.
 SPECIAL-NAMES.
         SYSOUT        IS        SOUT
         STATION       IS        STA
         CONSOLE       IS        CSL.
*
 INPUT-OUTPUT                    SECTION.
 FILE-CONTROL.
*－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－*
*            画面ファイル　　　　　　　　　　　　　　　　　　  *
*－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－－*
     SELECT    DSPF       ASSIGN       TO   GS-DSPF
               SYMBOLLIC  DESTINATION  IS   "DSP"
               FORMAT                  IS   DSP-FORMAT
               GROUP                   IS   DSP-GROUP
               PROCESSING              IS   DSP-PROC
               FUNCTION                IS   DSP-FUNC
               CONTROL                 IS   DSP-CONT
               STATUS                  IS   DSP-ST.
****************************************************************
 DATA                            DIVISION.
****************************************************************
 FILE                            SECTION.
*---< 表示ファイル >-------*
 FD  DSPF              LABEL     RECORD    IS   STANDARD.
     COPY    FPAUSE  OF        XMDLIB.
****************************************************************
 WORKING-STORAGE                 SECTION.
****************************************************************
 01  PGM-ID                  PIC  X(08)    VALUE     "SHOM0900".
*
 01  FLAGS.
     03  END-FLG             PIC  X(03)    VALUE  SPACE.
*
 01  SYS-DATE.
     03  SYS-DATE-YY         PIC  9(02)    VALUE  ZERO.
     03  SYS-DATE-MM         PIC  9(02)    VALUE  ZERO.
     03  SYS-DATE-DD         PIC  9(02)    VALUE  ZERO.
 01  SYS-TIME.
     03  SYS-TIME-HH         PIC  9(04)    VALUE  ZERO.
     03  SYS-TIME-MM         PIC  9(02)    VALUE  ZERO.
     03  SYS-TIME-SS         PIC  9(02)    VALUE  ZERO.
*
*画面制御用
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-FUNC            PIC  X(04).
     03  DSP-CONT            PIC  X(06).
     03  DSP-ST              PIC  X(02)    VALUE  SPACE.
 LINKAGE                         SECTION.
 01  LINK-MSG1               PIC  N(25).
 01  LINK-MSG2               PIC  N(25).
 01  LINK-MSG3               PIC  N(25).
 01  LINK-MSG4               PIC  N(25).
 01  LINK-MSG5               PIC  N(25).
****************************************************************
 PROCEDURE                   DIVISION      USING  LINK-MSG1
                                                  LINK-MSG2
                                                  LINK-MSG3
                                                  LINK-MSG4
                                                  LINK-MSG5.
****************************************************************
 DECLARATIVES.
*---------<  画面ファイル　>-----------------------------------*
 DSPF-ERR-SECTION               SECTION.
        USE     AFTER     EXCEPTION   PROCEDURE  DSPF.
        DISPLAY "### " PGM-ID " DSPF        ERROR " DSP-ST " "
             SYS-DATE-YY "." SYS-DATE-MM "." SYS-DATE-DD " "
             SYS-TIME-HH ":" SYS-TIME-MM ":" SYS-TIME-SS " ###"
             UPON CSL.
        STOP      RUN.
 END    DECLARATIVES.
****************************************************************
*    < 0.0 >           基本モジュール
****************************************************************
 PROGRAM-START-SEC           SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC   UNTIL   END-FLG  =  "END".
     PERFORM      FIN-SEC.
     STOP         RUN.
 PROGRAM-END-EXIT.
     EXIT.
****************************************************************
*    < 1.0 >           初期処理
****************************************************************
 INIT-SEC                    SECTION.
* ファイルオープン
     ACCEPT  SYS-DATE   FROM   DATE.
     ACCEPT  SYS-TIME   FROM   TIME.
     OPEN    I-O    DSPF.
 INIT-EXT.
     EXIT.
****************************************************************
*    < 2.0 >           主　処　理
****************************************************************
 MAIN-SEC                   SECTION.
*初期画面表示
     PERFORM   DSP-INIT-SEC.
 MAIN-010.
     PERFORM   DSP-READ-SEC
     EVALUATE  DSP-FUNC
         WHEN  "F009"
               MOVE      "END"          TO   END-FLG
               MOVE      NC"自動なし"   TO   OPRMSG
               MOVE      0155           TO   PROGRAM-STATUS
         WHEN  "E000"
               MOVE      "END"          TO   END-FLG
               MOVE      NC"自動出力"   TO   OPRMSG
               MOVE      ZERO           TO   PROGRAM-STATUS
     END-EVALUATE.
     PERFORM   DSP-WRITE-SEC.
 MAIN-EXIT.
     EXIT.
****************************************************************
*    < 3.0 >           終了処理
****************************************************************
 FIN-SEC                     SECTION.
     CLOSE          DSPF.
 FIN-EXIT.
     EXIT.
****************************************************************
*    < ALL >      初期画面表示
****************************************************************
 DSP-INIT-SEC                SECTION.
     MOVE    LOW-VALUE           TO   FPAUSE.
     MOVE   "FPAUSE"           TO   DSP-FORMAT.
*    MOVE   "CL"                 TO   DSP-PROC.
     MOVE    LINK-MSG1           TO   MSG1.
     MOVE    LINK-MSG2           TO   MSG2.
     MOVE    LINK-MSG3           TO   MSG3.
     MOVE    LINK-MSG4           TO   MSG4.
     MOVE    LINK-MSG5           TO   MSG5.
     PERFORM DSP-WRITE-SEC.
 DSP-INIT-EXT.
     EXIT.
****************************************************************
*    < ALL >      画面表示処理
****************************************************************
 DSP-WRITE-SEC              SECTION.
     MOVE     SPACE         TO    DSP-PROC.
     MOVE    "SCREEN"       TO    DSP-GROUP.
     WRITE    FPAUSE.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*    < ALL >      画面入力処理
****************************************************************
 DSP-READ-SEC               SECTION.
     MOVE    "KAKEND"       TO   DSP-GROUP.
     MOVE    "NE"           TO   DSP-PROC.
     READ    DSPF       AT  END
                            GO   TO    DSP-READ-EXIT
     END-READ.
     IF    DSP-ST    NOT =  ZERO
           GO  TO    DSP-READ-SEC
     END-IF.
 DSP-READ-EXIT.
     EXIT.
