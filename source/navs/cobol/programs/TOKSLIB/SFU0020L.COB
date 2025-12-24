****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）                    *
*    業務名　　　　　：　在庫管理システム                      *
*    モジュール名　　：　振替依頼入力チェックリスト            *
*    作成日　　　　　：  00/06/19                              *
*    作成者　　　　　：  NAV                                   *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU0020L.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          00/06/19.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE       IS   CONS
     STATION       IS   STA
         YB        IS   1-5PITCH
         YA        IS   2PITCH
         YB-21     IS   3PITCH.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 振替依頼ファイル >>-*
     SELECT   FUIRAIF   ASSIGN    TO        DA-01-VI-FUIRAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       FIR-F01   FIR-F02
                                            FIR-F03
                        FILE      STATUS    FIR-ST1.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F011   MEI-F0121
                                            MEI-F0122  MEI-F0123
                        FILE      STATUS    MEI-ST1.
*----<< プリントファイル >>-*
     SELECT   PRTFILE   ASSIGN    TO        LP-04.
*----<< 画面ファイル >>-*
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        SYMBOLIC  DESTINATION        "DSP"
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING  MODE    DSP-PRO
                        SELECTED  FUNCTION  DSP-FNC
                        FILE      STATUS    DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 振替依頼ファイル >>-*
 FD  FUIRAIF            LABEL     RECORD     IS  STANDARD.
     COPY     FUIRAIF   OF   XFDLIB    JOINING   FIR  AS   PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            LABEL     RECORD     IS  STANDARD.
     COPY     JYOKEN1   OF   XFDLIB    JOINING   JYO  AS   PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS    OF   XFDLIB    JOINING   SOK  AS   PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS   PREFIX.
*----<< プリントファイル >>-*
 FD    PRTFILE          LINAGE    IS   66.
 01    PRT-REC                    PIC  X(200).
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     FFU00201   OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)  VALUE  "SFU0020L".
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT             PIC  X(08)  VALUE  SPACE.
     03  DSP-GRP             PIC  X(08)  VALUE  SPACE.
     03  DSP-PRO             PIC  X(02)  VALUE  SPACE.
     03  DSP-FNC             PIC  X(04)  VALUE  SPACE.
     03  DSP-ST.
       05  DSP-ST1           PIC  X(02)  VALUE  SPACE.
       05  DSP-ST2           PIC  X(04)  VALUE  SPACE.
     03  DSP-CON             PIC  X(06)  VALUE  SPACE.
     03  WK-GRP.
       05  WK-GRP-BODY       PIC  X(04)  VALUE  SPACE.
       05  WK-GRP-LINE       PIC  9(02)  VALUE  ZERO.
       05  FILLER            PIC  X(02)  VALUE  SPACE.
*----<< ｽﾃｰﾀｽ ｴﾘｱ >>-*
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  FIR-ST.
         05  FIR-ST1         PIC  X(02)  VALUE  SPACE.
         05  FIR-ST2         PIC  X(04)  VALUE  SPACE.
     03  JYO-ST.
         05  JYO-ST1         PIC  X(02)  VALUE  SPACE.
         05  JYO-ST2         PIC  X(04)  VALUE  SPACE.
     03  SOK-ST.
         05  SOK-ST1         PIC  X(02)  VALUE  SPACE.
         05  SOK-ST2         PIC  X(04)  VALUE  SPACE.
     03  MEI-ST.
         05  MEI-ST1         PIC  X(02)  VALUE  SPACE.
         05  MEI-ST2         PIC  X(04)  VALUE  SPACE.
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消　_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消　_終了　_項目戻り".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました。".
     03  MSG02               PIC  N(20)  VALUE
            NC"Ｙを入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     03  MSG04               PIC  N(20)  VALUE
            NC"年月日に誤りがあります。".
     03  MSG05               PIC  N(20)  VALUE
            NC"開始が終了を越えています。".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       5.
*----<< ﾜｰｸ ｴﾘｱ >>-*
 01  CNT-AREA.
     03  LINE-CNT            PIC  9(02)  VALUE  99.
     03  PAGE-CNT            PIC  9(07)  VALUE  ZERO.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE  ZERO.
 01  KEY-AREA.
     03  WK-SNO              PIC  9(07)  VALUE  ZERO.
*----<< ﾋﾂﾞｹﾁｪｯｸ ﾜｰｸ ｴﾘｱ >>-*
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
*
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  SHORI-F             PIC  9(02)  VALUE  ZERO.
*日付／時刻
 01  SYS-DATE                PIC  9(08).
 01  FILLER                  REDEFINES   SYS-DATE.
     03  FILLER              PIC  9(02).
     03  SYS-YMD.
       05  SYS-YY            PIC  9(02).
       05  SYS-MM            PIC  9(02).
       05  SYS-DD            PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
****  見出し行０             ****
 01  MIDASI-0.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  X(08)  VALUE  "SFU0020L".
     03  FILLER              PIC  X(07)  VALUE  SPACE.
     03  FILLER              PIC  N(21)  VALUE
         NC"＊＊＊　振替依頼入力チェックリスト　＊＊＊"
                             CHARACTER   TYPE   IS   3PITCH.
     03  H-TOKHAN            PIC  N(08)  VALUE  SPACE
                             CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"処理日"
                             CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-YY                PIC  9(04).
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  H-MM                PIC  Z9.
     03  FILLER              PIC  X(01)  VALUE  ".".
     03  H-DD                PIC  Z9.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"頁"
                             CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-PAGE              PIC  ZZZ9.
****  見出し行１             ****
 01  MIDASI-1.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"［営業所："
                             CHARACTER   TYPE   IS   2PITCH.
     03  H1-SOKCD            PIC  X(02)  VALUE  SPACE.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  H1-SOKNM            PIC  N(15)
                             CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  N(01)  VALUE  NC"］"
                             CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  X(64)  VALUE  SPACE.
     03  FILLER              PIC  N(08)  VALUE
                             NC"（★：依頼する）"
                             CHARACTER   TYPE   IS   1-5PITCH.
****  見出し行２             ****
 01  MIDASI-2                CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  X(04)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"振替依頼_".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"出荷希望日".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"振替元".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"振替先".
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(02)  VALUE  NC"商品".
     03  FILLER              PIC  X(05)  VALUE  "ｺｰﾄﾞ ".
     03  FILLER              PIC  N(03)  VALUE  NC"品　単".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(05)  VALUE  NC"商　品　名".
     03  FILLER              PIC  X(36)  VALUE  SPACE.
     03  FILLER              PIC  N(06)  VALUE
                             NC"振替依頼数量".
****  見出し行３             ****
 01  MIDASI-3                CHARACTER   TYPE   IS   2PITCH.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  N(67)  VALUE  ALL NC"─".
****  明細行１               ****
 01  MEISAI-1.
     03  FILLER              PIC  X(01).
     03  M-IRIKBN            PIC  N(01)
                             CHARACTER    TYPE   IS   2PITCH.
     03  FILLER              PIC  X(01).
     03  M-IRINO             PIC  9(07).
     03  FILLER              PIC  X(01).
     03  M-GYONO-1           PIC  9.
     03  FILLER              PIC  X(02).
     03  M-SYUYMD-YY         PIC  9999.
     03  M-TEN1              PIC  X(01).
     03  M-SYUYMD-MM         PIC  99.
     03  M-TEN2              PIC  X(01).
     03  M-SYUYMD-DD         PIC  99.
     03  FILLER              PIC  X(03).
     03  M-FMOTO             PIC  X(02).
     03  FILLER              PIC  X(05).
     03  M-FSAKI             PIC  X(02).
     03  FILLER              PIC  X(03).
     03  M-HINCD-1           PIC  X(08).
     03  FILLER              PIC  X(01).
     03  M-HINTAN-1          PIC  X(08).
     03  FILLER              PIC  X(01).
     03  M-HINMEI1-1         PIC  N(15)
                             CHARACTER    TYPE   IS   1-5PITCH.
     03  M-HINMEI2-1         PIC  N(13)
                             CHARACTER    TYPE   IS   1-5PITCH.
     03  M-SURYO-1           PIC  ---,---,--9.99.
 01  MEISAI-2.
     03  FILLER              PIC  X(12).
     03  M-GYONO             PIC  9.
     03  FILLER              PIC  X(27).
     03  M-HINCD             PIC  X(08).
     03  FILLER              PIC  X(01).
     03  M-HINTAN            PIC  X(08).
     03  FILLER              PIC  X(01).
     03  M-HINMEI1           PIC  N(15)
                             CHARACTER    TYPE   IS   1-5PITCH.
     03  M-HINMEI2           PIC  N(13)
                             CHARACTER    TYPE   IS   1-5PITCH.
     03  M-SURYO             PIC  ---,---,--9.99.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*---<< LINK AREA >>-*
 LINKAGE                   SECTION.
 01  LINK-SOKCD              PIC  X(02).
 01  LINK-DAISOKCD           PIC  X(02).
*
******************************************************************
 PROCEDURE         DIVISION      USING     LINK-SOKCD
                                           LINK-DAISOKCD.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  振替依頼ファイル　--------------------------------*
 FIR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   FUIRAIF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"振替依頼ファイル異常！"
              "ST1=" FIR-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   条件ファイル　-----------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   倉庫マスタ -----------------------------------*
 SOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSOKMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"倉庫マスタ異常！"
              "ST1=" SOK-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STA.
*
     ACCEPT   IN-DATA        FROM STA.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     DISPLAY  "**  SFU0020L   START  **"   UPON  CONS.
*
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG  =   "END".
     PERFORM  END-RTN.
*
     DISPLAY  "**  SFU0020L    END   **"   UPON  CONS.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-RTN           SECTION.
     ACCEPT      SYS-YMD     FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*****DISPLAY  "*** " PGM-ID " START "
*****         SYS-YY "." SYS-MM "." SYS-DD " "
*****         SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STA.
* ファイル　ＯＰＥＮ
     OPEN     INPUT     FUIRAIF.
     OPEN     INPUT     HJYOKEN.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
*システム日付・時刻の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY H-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM   H-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD   H-DD.
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03            TO   HEN-TOKHAN
     END-READ.
     MOVE     HEN-TOKHAN-AREA     TO   H-TOKHAN.
*
     MOVE     0              TO   SHORI-F.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-RTN           SECTION.
     EVALUATE    SHORI-F
       WHEN      0      PERFORM   DSP-INIT-RTN
       WHEN      1      PERFORM   DSP-GRP003-RTN
       WHEN      2      PERFORM   DSP-KAKNIN-RTN
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-RTN            SECTION.
     CLOSE              FUIRAIF
                        HJYOKEN
                        ZSOKMS
                        HMEIMS
                        DSPFILE
                        PRTFILE.
*
     DISPLAY "* FUIRAIF (IN)=" IN-CNT   " *" UPON CONS.
     DISPLAY "* FUIRAIF (OT)=" OUT-CNT  " *" UPON CONS.
     DISPLAY "* PRINTF(PAGE)=" PAGE-CNT " *" UPON CONS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*****DISPLAY  "*** " PGM-ID " END   "
*****         SYS-YY "." SYS-MM "." SYS-DD " "
*****         SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STA.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    初期画面表示処理                            *
*--------------------------------------------------------------*
 DSP-INIT-RTN           SECTION.
     MOVE     SPACE          TO   FFU00201.
     IF       LINK-SOKCD          =   "01"  AND
              LINK-DAISOKCD       =   "01"
              CONTINUE
     ELSE
              MOVE     LINK-SOKCD    TO   SOK-F01  SOKOCD
              PERFORM  SOK-READ-RTN
              IF       INV-FLG   =    1
                    CONTINUE
              ELSE
                    MOVE   SOK-F02   TO   SOKONM
              END-IF
     END-IF.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
*属性クリア
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE     "FFU00201"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
*
     MOVE     1              TO   SHORI-F.
*
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　入力                              *
*--------------------------------------------------------------*
 DSP-GRP003-RTN     SECTION.
     MOVE     G001           TO   GUIDE.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "E000"
              PERFORM   CHK-BODY-RTN
              IF  ERR-FLG         =    ZERO
                  MOVE  "Y"       TO   KAKNIN
                  MOVE  2         TO   SHORI-F
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-GRP003-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　チェック                          *
*--------------------------------------------------------------*
 CHK-BODY-RTN       SECTION.
*属性クリア
     PERFORM  CLR-BODY-RTN.
     MOVE     ZERO     TO    ERR-FLG.
*倉庫チェック
     IF     ( LINK-SOKCD     =   "01" ) AND
            ( LINK-DAISOKCD  =   "01" )
              CONTINUE
     ELSE
              IF    SOKOCD      NOT =   LINK-SOKCD
                    MOVE    3        TO   ERR-FLG
                    MOVE   "C"       TO   EDIT-CURSOR OF SOKOCD
                    MOVE   "R"       TO   EDIT-OPTION OF SOKOCD
              END-IF
     END-IF.
     IF       SOKOCD  =   SPACE
              MOVE     NC"全倉庫"    TO   SOKONM
     ELSE
              MOVE     SOKOCD        TO   SOK-F01
              PERFORM  SOK-READ-RTN
              IF       INV-FLG   =    1
                    MOVE    3        TO   ERR-FLG
                    MOVE   "C"       TO   EDIT-CURSOR OF SOKOCD
                    MOVE   "R"       TO   EDIT-OPTION OF SOKOCD
                    MOVE   SPACE     TO   SOKONM
              ELSE
                    MOVE   SOK-F02   TO   SOKONM
              END-IF
     END-IF.
*
 CHK-BODY-100.
*
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KAKNIN-RTN      SECTION.
     PERFORM  CLR-BODY-RTN.
     MOVE     G002                TO   GUIDE.
     MOVE     "GRP002"            TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SHORI-F
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              PERFORM   CLR-TAIL-RTN
              MOVE      1         TO   SHORI-F
     WHEN     "E000"
              PERFORM   CHK-KAKNIN-RTN
              IF   ERR-FLG   =    0
                   PERFORM   PRT-WR-RTN
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　チェック                              *
*--------------------------------------------------------------*
 CHK-KAKNIN-RTN       SECTION.
*属性クリア
     PERFORM  CLR-BODY-RTN.
*
     IF       KAKNIN   NOT =      "Y"
              MOVE      2    TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KAKNIN
     END-IF.
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     画面ＲＥＡＤ　　　　                        *
*--------------------------------------------------------------*
 DSP-RD-RTN           SECTION.
     IF       ERR-FLG   =    0
              MOVE      SPACE              TO   GMSG
     ELSE
              MOVE      MSG-TBL(ERR-FLG)   TO   GMSG
     END-IF.
     MOVE     "SCREEN"  TO      DSP-GRP.
     PERFORM  DSP-WR-RTN.
*
     IF       ERR-FLG   NOT =   0
              MOVE      "AL"    TO   DSP-PRO
              MOVE      0       TO   ERR-FLG
     ELSE
              MOVE      "NE"    TO   DSP-PRO
     END-IF.
*
     MOVE     WK-GRP            TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE             TO   DSP-PRO.
 DSP-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL      画面ＷＲＩＴＥ　                           *
*--------------------------------------------------------------*
 DSP-WR-RTN             SECTION.
     MOVE     SPACE        TO   DSP-PRO.
     WRITE    FFU00201.
 DSP-WR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF SOKOCD.
     MOVE     "M"            TO   EDIT-OPTION OF SOKOCD.
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF KAKNIN.
     MOVE     "M"            TO   EDIT-OPTION OF KAKNIN.
 CLR-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3          帳票出力処理                            *
*--------------------------------------------------------------*
 PRT-WR-RTN             SECTION.
     PERFORM       FIR-READ-RTN.
     IF       END-FLG    NOT =      "END"
              MOVE     SPACE         TO   MEISAI-1
              MOVE     SPACE         TO   MEISAI-2
     END-IF.
     PERFORM       HENSYU-RTN   UNTIL  END-FLG = "END".
 PRT-WR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3         編集印字　処理                           *
*--------------------------------------------------------------*
 HENSYU-RTN             SECTION.
     IF       LINE-CNT      >=      62      OR
              FIR-F01     NOT =     H1-SOKCD
              PERFORM       MIDASI-RTN
     END-IF.
     IF       WK-SNO         =     FIR-F02   AND
              LINE-CNT   NOT =     6
       PERFORM     TENSO1-RTN
       WRITE       PRT-REC   FROM      MEISAI-2   AFTER  1
       MOVE        SPACE     TO        MEISAI-2
       ADD         1         TO        LINE-CNT
      ELSE
       PERFORM     TENSO2-RTN
       IF       LINE-CNT   <=       6
              WRITE    PRT-REC   FROM      MEISAI-1   AFTER 1
              MOVE     SPACE     TO        MEISAI-1
              ADD      1         TO        LINE-CNT
       ELSE
              WRITE    PRT-REC   FROM      MEISAI-1   AFTER 2
              MOVE     SPACE     TO        MEISAI-1
              ADD      2         TO        LINE-CNT
       END-IF
      MOVE     FIR-F02       TO        WK-SNO
     END-IF.
*
     PERFORM       FIR-READ-RTN.
*
 HENSYU-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細１転送処理　                            *
*--------------------------------------------------------------*
*明細転送
 TENSO1-RTN             SECTION.
     PERFORM  TENSO3-RTN.
 TENSO1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細２転送処理　                            *
*--------------------------------------------------------------*
 TENSO2-RTN             SECTION.
     IF            LINE-CNT       =    61
         PERFORM   MIDASI-RTN
     END-IF.
     IF       FIR-F10        =    "1"
              MOVE      NC"★"    TO   M-IRIKBN
     ELSE
              MOVE      SPACE     TO   M-IRIKBN
     END-IF.
     MOVE     FIR-F02        TO   M-IRINO.
     MOVE     FIR-F03        TO   M-GYONO.
     IF       FIR-F04   NOT  =    0      OR
              FIR-F04   NOT  =    SPACE
         MOVE      "."       TO   M-TEN1
         MOVE      "."       TO   M-TEN2
     END-IF.
     MOVE     FIR-F04(1:4)   TO   M-SYUYMD-YY.
     MOVE     FIR-F04(5:2)   TO   M-SYUYMD-MM.
     MOVE     FIR-F04(7:2)   TO   M-SYUYMD-DD.
     MOVE     FIR-F05        TO   M-FMOTO.
     MOVE     FIR-F06        TO   M-FSAKI.
     PERFORM  TENSO3-1-RTN.
 TENSO2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細３転送処理　                            *
*--------------------------------------------------------------*
 TENSO3-RTN             SECTION.
     MOVE     FIR-F03        TO   M-GYONO.
     MOVE     FIR-F07        TO   M-HINCD.
     MOVE     FIR-F08        TO   M-HINTAN.
*商品名索引
     MOVE     FIR-F07        TO   MEI-F011.
     MOVE     FIR-F08(1:5)   TO   MEI-F0121.
     MOVE     FIR-F08(6:2)   TO   MEI-F0122.
     MOVE     FIR-F08(8:1)   TO   MEI-F0123.
     PERFORM  MEI-READ-RTN.
     IF       INV-FLG      =      0
              MOVE    MEI-F021    TO        M-HINMEI1
              MOVE    MEI-F022    TO        M-HINMEI2
     ELSE
              MOVE    SPACE       TO        M-HINMEI1
              MOVE    SPACE       TO        M-HINMEI2
     END-IF.
     MOVE     FIR-F09        TO   M-SURYO.
 TENSO3-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      明細３－１転送処理                          *
*--------------------------------------------------------------*
 TENSO3-1-RTN           SECTION.
     MOVE     FIR-F03        TO   M-GYONO-1.
     MOVE     FIR-F07        TO   M-HINCD-1.
     MOVE     FIR-F08        TO   M-HINTAN-1.
*商品名索引
     MOVE     FIR-F07        TO   MEI-F011.
     MOVE     FIR-F08(1:5)   TO   MEI-F0121.
     MOVE     FIR-F08(6:2)   TO   MEI-F0122.
     MOVE     FIR-F08(8:1)   TO   MEI-F0123.
     PERFORM  MEI-READ-RTN.
     IF       INV-FLG      =      0
              MOVE    MEI-F021    TO        M-HINMEI1-1
              MOVE    MEI-F022    TO        M-HINMEI2-1
     ELSE
              MOVE    SPACE       TO        M-HINMEI1-1
              MOVE    SPACE       TO        M-HINMEI2-1
     END-IF.
     MOVE     FIR-F09        TO   M-SURYO-1.
 TENSO3-1-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.1       見出し処理                              *
*----------------------------------------------------------*
 MIDASI-RTN             SECTION.
     MOVE     SPACE          TO      PRT-REC.
     ADD      1              TO      PAGE-CNT.
     MOVE     PAGE-CNT       TO      H-PAGE.
*
     MOVE     FIR-F01        TO      SOK-F01  H1-SOKCD.
     PERFORM  SOK-READ-RTN.
     IF       INV-FLG   =    0
              MOVE   SOK-F02         TO   H1-SOKNM
     ELSE
              MOVE   SPACE           TO   H1-SOKNM
     END-IF.
*
     IF     PAGE-CNT      NOT =      1
            WRITE  PRT-REC   AFTER   PAGE
     END-IF.
     WRITE         PRT-REC   FROM    MIDASI-0   AFTER  2.
     WRITE         PRT-REC   FROM    MIDASI-1   AFTER  1.
     WRITE         PRT-REC   FROM    MIDASI-3   AFTER  1.
     WRITE         PRT-REC   FROM    MIDASI-2   AFTER  1.
     WRITE         PRT-REC   FROM    MIDASI-3   AFTER  1.
     MOVE          6         TO      LINE-CNT.
 MIDASI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品名称マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 MEI-READ-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HMEIMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-READ-RTN           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   倉庫マスタ　　　ＲＥＡＤ                     *
*--------------------------------------------------------------*
 SOK-READ-RTN           SECTION.
     MOVE     0              TO   INV-FLG.
     READ     ZSOKMS   INVALID  KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   振替依頼マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 FIR-READ-RTN           SECTION.
     READ     FUIRAIF
         AT   END
              MOVE      "END"     TO   END-FLG
              GO        TO        FIR-READ-EXIT
     END-READ.
     ADD      1         TO        IN-CNT.
*出力判定
     IF       SOKOCD    NOT =     SPACE     AND
              SOKOCD    NOT =     FIR-F01
              ADD       1         TO   SKIP-CNT
              GO        TO        FIR-READ-RTN
     END-IF.
     ADD      1         TO   OUT-CNT.
 FIR-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
