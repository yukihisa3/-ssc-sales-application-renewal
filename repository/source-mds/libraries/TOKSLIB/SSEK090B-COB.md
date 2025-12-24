# SSEK090B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSEK090B.COB`

## ソースコード

```cobol
********************************************************
*       　   セキチューオンライン                      *
*              商品台帳発行　                          *
*       商品台帳データより商品台帳を発行する           *
********************************************************
**********************************************************
 IDENTIFICATION         DIVISION.
**********************************************************
 PROGRAM-ID.            SSEK090B.
 AUTHOR.                Y.M.
 DATE-WRITTEN.          96/04/22.
**********************************************************
 ENVIRONMENT            DIVISION.
**********************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150.
 OBJECT-COMPUTER.       K-150.
 SPECIAL-NAMES.
     CONSOLE     IS     CONSL
     STATION     IS     STATION.
******************************************************************
*             INPUT-OUTPUT                                       *
******************************************************************
 INPUT-OUTPUT           SECTION.
******************************************************************
 FILE-CONTROL.
*商品台帳データ
     SELECT   SKSHOHIN  ASSIGN          DA-01-S-SKSHOHIN
                        ORGANIZATION    SEQUENTIAL
                        ACCESS    MODE  SEQUENTIAL
                        STATUS          SHI-ST   SHI-ST2.
*プリンタＦ
     SELECT   PRTFILE   ASSIGN          01-GS-PRTF
                        DESTINATION     "PRT"
                        FORMAT          PRT-FMT
                        GROUP           PRT-GRP
                        PROCESSING      PRT-PRO
                        CONTROL         PRT-CTL
                        STATUS          PRT-ST    PRT-ST2.
*表示ファイル
     SELECT   DSPFILE   ASSIGN    TO    01-GS-DSPF
                        ORGANIZATION    SEQUENTIAL
                        DESTINATION     "DSP"
                        FORMAT           DSP-FORM
                        GROUP            DSP-GRP
                        PROCESSING       DSP-PROC
                        FUNCTION         PF-KEY
                        CONTROL          DSP-CONTROL
                        STATUS           DSP-ST  DSP-ST2.
******************************************************************
 DATA                   DIVISION.
******************************************************************
 FILE                   SECTION.
********************************************************
*      商品台帳データ　   RL=256                           *
********************************************************
 FD  SKSHOHIN           BLOCK CONTAINS  4  RECORDS.
*
 01  SHOREC.
     03  SHO01                  PIC  X(01).
     03  SHO02                  PIC  X(02).
     03  SHO03                  PIC  9(03).
     03  SHO04                  PIC  9(03).
     03  SHO05                  PIC  9(03).
     03  SHO06                  PIC  9(03).
     03  SHO07                  PIC  X(08).
     03  SHO08                  PIC  X(13).
     03  SHO09                  PIC  X(20).
     03  SHO10                  PIC  X(20).
     03  SHO11                  PIC  X(01).
     03  SHO12                  PIC  X(01).
     03  SHO13                  PIC  X(01).
     03  SHO14                  PIC  X(01).
     03  SHO15                  PIC  9(04).
     03  SHO16                  PIC  9(04).
     03  SHO17                  PIC  X(01).
     03  SHO18                  PIC  X(01).
     03  SHO19                  PIC  9(06).
     03  SHO20                  PIC  9(06).
     03  SHO21                  PIC  X(01).
     03  SHO22                  PIC  X(25).
*
     03  SHO23                  PIC  X(01).
     03  SHO24                  PIC  X(02).
     03  SHO25.
         05  SHO25AB.
             07  SHO25A         PIC  X(03).
             07  SHO25B         PIC  X(02).
         05  SHO25CD.
             07  SHO25C         PIC  9(07)V99.
             07  SHO25D         PIC  9(07).
     03  SHO26.
         05  SHO26AB.
             07  SHO26A         PIC  X(03).
             07  SHO26B         PIC  X(02).
         05  SHO26CD.
             07  SHO26C         PIC  9(07)V99.
             07  SHO26D         PIC  9(07).
     03  SHO27.
         05  SHO27AB.
             07  SHO27A         PIC  X(03).
             07  SHO27B         PIC  X(02).
         05  SHO27CD.
             07  SHO27C         PIC  9(07)V99.
             07  SHO27D         PIC  9(07).
     03  SHO28.
         05  SHO28AB.
             07  SHO28A         PIC  X(03).
             07  SHO28B         PIC  X(02).
         05  SHO28CD.
             07  SHO28C         PIC  9(07)V99.
             07  SHO28D         PIC  9(07).
     03  SHO29.
         05  SHO29AB.
             07  SHO29A         PIC  X(03).
             07  SHO29B         PIC  X(02).
         05  SHO29CD.
             07  SHO29C         PIC  9(07)V99.
             07  SHO29D         PIC  9(07).
     03  SHO30.
         05  SHO30A             PIC  X(01).
         05  SHO30B             PIC  X(01).
         05  SHO30C             PIC  X(01).
         05  SHO30D             PIC  X(01).
         05  SHO30E             PIC  X(01).
     03  SHIFIL                 PIC  X(15).
********************************************************
*    プリンタ                                          *
********************************************************
 FD  PRTFILE.
     COPY    SSEK090L  OF        XMDLIB
             JOINING   PRT  AS   PREFIX.
********************************************************
*    表示ファイル                                      *
********************************************************
 FD  DSPFILE.
     COPY    SSEK090   OF        XMDLIB
             JOINING   DSP  AS   PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*画面パラメータ
 01  FORM-PARA.
     03  DSP-FORM                 PIC  X(08).
     03  DSP-PROC                 PIC  X(02).
     03  DSP-GRP                  PIC  X(08).
     03  PF-KEY                   PIC  X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL            PIC  X(04).
         05  DSP-STR-PG           PIC  X(02).
     03  DSP-ST                   PIC  X(02).
     03  DSP-ST2                  PIC  X(04).
*帳票パラメータ
 01  PRT-AREA.
     03  PRT-FMT            PIC X(08).
     03  PRT-PRO            PIC X(02).
     03  PRT-GRP            PIC X(08).
     03  PRT-CTL.
         05  PRT-CNTRL      PIC X(04).
         05  PRT-STR-PG     PIC X(02).
     03  PRT-ST             PIC  X(02).
     03  PRT-ST2            PIC  X(04).
*ステータス
 01  FILE-STATUS.
     03  SHI-ST                   PIC  X(02).
     03  SHI-ST2                  PIC  X(04).
 01  WK-AREA.
     03  END-FLG                  PIC  X(03).
     03  PAGE-CNT                 PIC  9(05).
     03  LINE-CNT                 PIC  9(05).
     03  SHORI-FLG                PIC  X(04).
     03  WK-JANCD                 PIC  X(13).
     03  IN-DATA                  PIC  X(01).
*    03  SYSDATE                  PIC  9(06).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
 01  FILE-ERR.
     03  SHIERR              PIC  N(15)    VALUE
         NC"商品台帳データ　異常".
     03  DSPERR              PIC  N(15)    VALUE
         NC"画面ファイル　異常".
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(20) VALUE
              NC"　　　　　ＰＦキーが違います　　　　　　".
     03  WK-ERR2                  PIC  N(20) VALUE
              NC"　　　　　　『テスト印字中』　　　　　　".
     03  WK-ERR3                  PIC  N(20) VALUE
              NC"　　　　　『商品台帳作成中』　　　　　　".
     03  WK-ERR4                  PIC  N(20) VALUE
              NC"　　　　　選択番号が違います　　　　　　".
     03  WK-ERR5                  PIC  N(20) VALUE
              NC"　　　　　対象伝票がありません　　　　　".
     03  WK-ERR6                  PIC  N(20) VALUE
              NC"　　開始・終了の指定に誤りがあります　　".
*
 01  WK-MEISAI-SU            PIC  9(06)    VALUE   ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 SHI-ERR           SECTION.
     USE AFTER     EXCEPTION      PROCEDURE SKSHOHIN.
     DISPLAY       SHIERR         UPON      STATION.
     DISPLAY       SHI-ST         UPON      STATION.
     DISPLAY       SHI-ST2        UPON      STATION.
     ACCEPT        IN-DATA        FROM      STATION.
     STOP  RUN.
 DSP-ERR           SECTION.
     USE AFTER     EXCEPTION      PROCEDURE DSPFILE.
     DISPLAY       DSPERR         UPON      STATION.
     DISPLAY       DSP-ST         UPON      STATION.
     DISPLAY       DSP-ST2        UPON      STATION.
     ACCEPT        IN-DATA        FROM      STATION.
     STOP  RUN.
 PRT-ERR           SECTION.
     USE AFTER     EXCEPTION      PROCEDURE PRTFILE.
     DISPLAY       "ﾌﾟﾘﾝﾀ ｴﾗｰ]]"  UPON      STATION.
     DISPLAY       PRT-ST         UPON      STATION.
     DISPLAY       PRT-ST2        UPON      STATION.
     ACCEPT        IN-DATA        FROM      STATION.
     STOP  RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC               SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL   END-FLG =   "END".
     PERFORM     END-SEC.
     STOP  RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC               SECTION.
     OPEN    INPUT       SKSHOHIN.
     OPEN    I-O         DSPFILE.
     OPEN    OUTPUT      PRTFILE.
*
     INITIALIZE  WK-AREA.
*    ACCEPT      SYSDATE         FROM    DATE.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
     MOVE   "INIT"               TO  SHORI-FLG.
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC               SECTION.
     EVALUATE    SHORI-FLG
         WHEN   "INIT"       PERFORM DSP-INIT-SEC
         WHEN   "BANG"       PERFORM BANGOU-SEC
         WHEN   "HANI"       PERFORM HANI-SEC
         WHEN   "LIST"       PERFORM LIST-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                SECTION.
     IF     (PAGE-CNT    NOT =   ZERO)
             PERFORM SEN-WRITE-SEC
     END-IF.
*ファイルのＣＬＯＳＥ
     CLOSE    SKSHOHIN.
     CLOSE    DSPFILE.
     CLOSE    PRTFILE.
*/////////////////////////////////////////////////////////////////
     DISPLAY "***  出力明細行数 = " WK-MEISAI-SU " 件  ***"
                                                   UPON  CONSL.
*/////////////////////////////////////////////////////////////////
 END-EXIT.
     EXIT.
***********************************************************
*               商品台帳データＲＥＡＤ                        *
***********************************************************
 SKSHOHIN-READ-SEC               SECTION.
*
*/////////////////////////////////////////////////////////////////
     IF    SHORI-FLG  =  "HANI"     OR    SHORI-FLG  =  "BANG"
           CLOSE           SKSHOHIN
           OPEN    INPUT   SKSHOHIN
     END-IF.
*/////////////////////////////////////////////////////////////////
*
 SKSHOHIN-READ-SEC-01.
*
     READ    SKSHOHIN
       AT  END
             MOVE   "END"         TO  END-FLG
             GO                   TO  SKSHOHIN-READ-EXIT
     END-READ.
*ＪＡＮコード比較
     IF     (SHO08   <   DSP-START)
             GO                   TO  SKSHOHIN-READ-SEC-01
     END-IF.
     IF     (SHO08   >   DSP-END)
             GO                   TO  SKSHOHIN-READ-SEC-01
     END-IF.
 SKSHOHIN-READ-EXIT.
     EXIT.
***********************************************************
*                     出力処理                        *
***********************************************************
 LIST-SEC               SECTION.
     MOVE    WK-ERR3             TO  DSP-ERRMSG.
     PERFORM DSP-WT-SEC.
*ＨＥＡＤ部　印刷
     IF     (SHO08   NOT =   WK-JANCD)
             PERFORM HEAD-MOVE-SEC
             PERFORM HEAD-WRITE-SEC
             MOVE    SHO08           TO  WK-JANCD
     END-IF.
*ＢＯＤＹ部　印刷
     IF      LINE-CNT  >=  60
             PERFORM HEAD-MOVE-SEC
             PERFORM HEAD-WRITE-SEC
     END-IF.
     PERFORM BODY-MOVE-SEC.
     PERFORM BODY-WRITE-SEC.
*商品台帳データのＲＥＡＤ
     PERFORM SKSHOHIN-READ-SEC.
 DENPYO-EXIT.
     EXIT.
**********************************************************
*                 見出し出力処理                         *
**********************************************************
 HEAD-MOVE-SEC              SECTION.
*ＴＡＩＴＯＬ部　印刷
     IF     (PAGE-CNT    =   ZERO)
     OR     (LINE-CNT   >=   60)
             PERFORM TAIT-WRITE-SEC
     END-IF.
*
     IF     (LINE-CNT    >   2)
             PERFORM SEN-WRITE-SEC
     END-IF.
*
     MOVE    SPACE               TO  PRT-HEADX.
*
     MOVE    SHO09               TO  PRT-SHONM.
     MOVE    SHO10               TO  PRT-KIKAKU.
     MOVE    SHO07               TO  PRT-SHOCD.
     MOVE    SHO08               TO  PRT-JANCD.
     MOVE    SHO03               TO  PRT-BUMON.
     MOVE    SHO04               TO  PRT-DAIBU.
     MOVE    SHO05               TO  PRT-CHUBU.
     MOVE    SHO06               TO  PRT-SHOBU.
     MOVE    SHO19               TO  PRT-TDATE.
     MOVE    SHO20               TO  PRT-HDATE.
     MOVE    SHO15               TO  PRT-HTANI.
     MOVE    SHO16               TO  PRT-NTANI.
     MOVE    SHO11               TO  PRT-SHOKU.
     MOVE    SHO12               TO  PRT-SOUMK.
     MOVE    SHO13               TO  PRT-LOT.
     MOVE    SHO14               TO  PRT-KIRKU.
     MOVE    SHO17               TO  PRT-ZEIKU.
     MOVE    SHO18               TO  PRT-RITKU.
 HEAD-MOVE-EXIT.
     EXIT.
*========================================================*
*                      印刷処理                         *
*========================================================*
 HEAD-WRITE-SEC       SECTION.
     MOVE   "HEAD"               TO  PRT-GRP.
     MOVE   "SSEK090L"           TO  PRT-FMT.
     MOVE   "PW"                 TO  PRT-PRO.
     MOVE   "A001"               TO  PRT-CTL.
     WRITE   PRT-SSEK090L.
*
     ADD     5                   TO  LINE-CNT.
 HEAD-WRITE-EXIT.
     EXIT.
*========================================================*
*                      印刷処理                         *
*========================================================*
 TAIT-WRITE-SEC       SECTION.
     ADD     1                   TO  PAGE-CNT.
     MOVE    WK-DATE             TO  PRT-SDATE.
     MOVE    PAGE-CNT            TO  PRT-PAGE.
*
     MOVE   "TAIT"               TO  PRT-GRP.
     MOVE   "SSEK090L"           TO  PRT-FMT.
     MOVE   "PW"                 TO  PRT-PRO.
     MOVE   "A000"               TO  PRT-CTL.
     WRITE   PRT-SSEK090L.
*
*    LINE-CNT の 初期化
     MOVE    ZERO                TO  LINE-CNT.
*
     MOVE    2                   TO  LINE-CNT.
 TAIT-WRITE-EXIT.
     EXIT.
*========================================================*
*                      印刷処理                         *
*========================================================*
 SEN-WRITE-SEC       SECTION.
     MOVE   "SENN"               TO  PRT-GRP.
     MOVE   "SSEK090L"           TO  PRT-FMT.
     MOVE   "PW"                 TO  PRT-PRO.
     MOVE   "A001"               TO  PRT-CTL.
     WRITE   PRT-SSEK090L.
*
     ADD     2                   TO  LINE-CNT.
 SEN-WRITE-EXIT.
     EXIT.
**********************************************************
*                     明細出力処理                       *
**********************************************************
 BODY-MOVE-SEC            SECTION.
     MOVE    SPACE               TO  PRT-BODYX.
*
     IF     (SHO25AB  =  SPACE  AND  SHO25CD  =  ZERO)
**** IF     (SHO25   =   SPACE)
             GO                  TO  BODY-MOVE-EXIT
     END-IF.
     MOVE    SHO30A              TO  PRT-TKBN(1).
     MOVE    SHO25A              TO  PRT-TENCD(1).
     MOVE    SHO25B              TO  PRT-ROUT(1).
     MOVE    SHO25C              TO  PRT-GENTAN(1).
     MOVE    SHO25D              TO  PRT-URITAN(1).
*
     IF     (SHO26AB  =  SPACE  AND  SHO26CD  =  ZERO)
**** IF     (SHO26   =   SPACE)
             GO                  TO  BODY-MOVE-EXIT
     END-IF.
     MOVE    SHO30B              TO  PRT-TKBN(2).
     MOVE    SHO26A              TO  PRT-TENCD(2).
     MOVE    SHO26B              TO  PRT-ROUT(2).
     MOVE    SHO26C              TO  PRT-GENTAN(2).
     MOVE    SHO26D              TO  PRT-URITAN(2).
*
     IF     (SHO27AB  =  SPACE  AND  SHO27CD  =  ZERO)
**** IF     (SHO27   =   SPACE)
             GO                  TO  BODY-MOVE-EXIT
     END-IF.
     MOVE    SHO30C              TO  PRT-TKBN(3).
     MOVE    SHO27A              TO  PRT-TENCD(3).
     MOVE    SHO27B              TO  PRT-ROUT(3).
     MOVE    SHO27C              TO  PRT-GENTAN(3).
     MOVE    SHO27D              TO  PRT-URITAN(3).
*
     IF     (SHO28AB  =  SPACE  AND  SHO28CD  =  ZERO)
**** IF     (SHO28   =   SPACE)
             GO                  TO  BODY-MOVE-EXIT
     END-IF.
     MOVE    SHO30D              TO  PRT-TKBN(4).
     MOVE    SHO28A              TO  PRT-TENCD(4).
     MOVE    SHO28B              TO  PRT-ROUT(4).
     MOVE    SHO28C              TO  PRT-GENTAN(4).
     MOVE    SHO28D              TO  PRT-URITAN(4).
*
     IF     (SHO29AB  =  SPACE  AND  SHO29CD  =  ZERO)
**** IF     (SHO29   =   SPACE)
             GO                  TO  BODY-MOVE-EXIT
     END-IF.
     MOVE    SHO30E              TO  PRT-TKBN(5).
     MOVE    SHO29A              TO  PRT-TENCD(5).
     MOVE    SHO29B              TO  PRT-ROUT(5).
     MOVE    SHO29C              TO  PRT-GENTAN(5).
     MOVE    SHO29D              TO  PRT-URITAN(5).
 BODY-MOVE-EXIT.
     EXIT.
*========================================================*
*                      印刷処理                         *
*========================================================*
 BODY-WRITE-SEC       SECTION.
     MOVE   "BODY"               TO  PRT-GRP.
     MOVE   "SSEK090L"           TO  PRT-FMT.
     MOVE   "PW"                 TO  PRT-PRO.
     MOVE   "A001"               TO  PRT-CTL.
     WRITE   PRT-SSEK090L.
*
     ADD     1                   TO  WK-MEISAI-SU.
     ADD     1                   TO  LINE-CNT.
 BODY-WRITE-EXIT.
     EXIT.
***********************************************************
*                    画面初期設定                         *
***********************************************************
 DSP-INIT-SEC               SECTION.
     CLOSE   PRTFILE.
     OPEN    OUTPUT      PRTFILE.
     MOVE    SPACE               TO  DSP-SSEK090.
     INITIALIZE  FORM-PARA.
     MOVE   "SSEK090"            TO  DSP-FORM.
     MOVE    HEN-DATE            TO  DSP-SDATE.
     MOVE    HEN-TIME            TO  DSP-STIME.
     PERFORM DSP-WT-SEC.
     MOVE   "BANG"               TO  SHORI-FLG.
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*            番号  　　　　　入力処理
****************************************************************
 BANGOU-SEC              SECTION.
     PERFORM DSP-WT-SEC.
     MOVE   "BANGOU"             TO  DSP-GRP.
     PERFORM DSP-RD-SEC.
     EVALUATE    PF-KEY
         WHEN   "F005"
                 MOVE   "END"            TO  END-FLG
         WHEN   "E000"
                 PERFORM CHK-BANGOU-SEC
         WHEN    OTHER
                 MOVE    WK-ERR1         TO  DSP-ERRMSG
     END-EVALUATE.
 BANGOU-EXIT.
     EXIT.
*==============================================================*
*            番号　　　　　　チェック処理                      *
*==============================================================*
 CHK-BANGOU-SEC                 SECTION.
     IF     (DSP-BANGOU  NOT NUMERIC)
             MOVE    ZERO            TO  DSP-BANGOU
     END-IF.
     EVALUATE    DSP-BANGOU
        WHEN     1
                 MOVE    WK-ERR4         TO  DSP-ERRMSG
        WHEN     2
                 MOVE    SPACE           TO  DSP-START
                 MOVE    ALL "9"         TO  DSP-END
                 PERFORM CHK-HANI-SEC
        WHEN     3
                 MOVE   "HANI"           TO  SHORI-FLG
                 MOVE    SPACE           TO  DSP-START
                                             DSP-END
        WHEN     OTHER
                 MOVE    WK-ERR4         TO  DSP-ERRMSG
     END-EVALUATE.
 CHK-BANGOU-EXIT.
     EXIT.
***************************************************************
*            範囲  　  　　　　入力処理
****************************************************************
 HANI-SEC              SECTION.
     PERFORM DSP-WT-SEC.
     MOVE   "HANI"               TO  DSP-GRP.
     PERFORM DSP-RD-SEC.
     EVALUATE    PF-KEY
         WHEN   "F004"
                 MOVE   "INIT"       TO  SHORI-FLG
         WHEN   "F005"
                 MOVE   "END"        TO  END-FLG
         WHEN   "E000"
                 PERFORM CHK-HANI-SEC
         WHEN    OTHER
                 MOVE    WK-ERR1     TO  DSP-ERRMSG
     END-EVALUATE.
 HANI-EXIT.
     EXIT.
*==============================================================*
*            範囲　　　　　　チェック処理                      *
*==============================================================*
 CHK-HANI-SEC                 SECTION.
*ＪＡＮコード
     IF      DSP-END  =   SPACE
             MOVE     ALL "9"         TO   DSP-END
     END-IF.
*エラー
     IF      DSP-START  >  DSP-END
             MOVE      WK-ERR6        TO   DSP-ERRMSG
             GO                       TO   CHK-HANI-EXIT
     END-IF.
*商品台帳データのＲＥＡＤ
     PERFORM SKSHOHIN-READ-SEC.
*対象データがない時
     IF      END-FLG   =   "END"
             MOVE      WK-ERR5        TO   DSP-ERRMSG
             IF    SHORI-FLG   =   "HANI"
                   MOVE     "HANI"    TO   SHORI-FLG
             ELSE
                   MOVE     "BANG"    TO   SHORI-FLG
             END-IF
             MOVE      SPACE          TO   END-FLG
     ELSE
             PERFORM   DSP-WT-SEC
             MOVE     "LIST"          TO   SHORI-FLG
     END-IF.
 CHK-HANI-EXIT.
     EXIT.
***********************************************************
*                       画面出力                          *
***********************************************************
 DSP-WT-SEC               SECTION.
     MOVE   "SCREEN"   TO        DSP-GRP.
     MOVE    SPACE     TO        DSP-PROC.
     WRITE   DSP-SSEK090.
     MOVE    SPACE     TO        DSP-ERRMSG.
 DSP-WT-EXIT.
     EXIT.
***********************************************************
*                      画面入力                           *
***********************************************************
 DSP-RD-SEC             SECTION.
     MOVE     "NE"      TO        DSP-PROC.
     READ     DSPFILE.
 DSP-RD-EXIT.
     EXIT.

```
