# SMNT041

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SMNT041.COB`

## ソースコード

```cobol
**********************************************************
*                                                        *
*             商品変換テーブルマスタメンテ               *
*                    93.04.12  UPDATE                    *
*   更新日    :  08/07/30 - 07/31 TAKEI
*                12/10/11 T.MIURA
*                13/02/18 M.INOUE
*   更新内容  :  内部統制対応
*                ＬＩＮＫＳ連携（ラベル張替区分追加）
*                ＬＩＮＫＳ連携（ラベル張替区分"2"入力可）
**********************************************************
 IDENTIFICATION                      DIVISION.
 PROGRAM-ID.               OMNT041.
 AUTHOR.                   INOUE.
 DATE-WRITTEN.             92/11/11.
 ENVIRONMENT                         DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          FACOM.
 OBJECT-COMPUTER.          FACOM.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS
     STATION     IS        STA.
*********************************************************
 INPUT-OUTPUT              SECTION.
*********************************************************
 FILE-CONTROL.
     SELECT      DSPF      ASSIGN    TO        GS-DSPF
                           FORMAT              DSP-FMT
                           GROUP               DSP-GRP
                           PROCESSING          DSP-PRO
                           FUNCTION            DSP-FNC
                           STATUS              DSP-ST.
     SELECT      HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           STATUS              TOK-ST.
     SELECT      HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       STB-F01   STB-F02
                           STATUS              STB-ST.
     SELECT      HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       MEI-F011
                                               MEI-F0121
                                               MEI-F0122
                                               MEI-F0123
                           STATUS              MEI-ST.
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                           STATUS              JYO-ST.
***## 2008/07/30
     SELECT      HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           STATUS              TAN-ST.
     SELECT      HSHOTBR   ASSIGN    TO        DA-01-VI-HSHOTBR1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       STR-F01
                                               STR-F02
                                               STR-F03
                           STATUS              STR-ST.
******************************************************************
 DATA                                DIVISION.
******************************************************************
 FILE                      SECTION.
 FD  HTOKMS
     LABEL       RECORD    IS        STANDARD
     BLOCK       CONTAINS  1         RECORDS.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
 FD  HSHOTBL
     LABEL       RECORD    IS        STANDARD
     BLOCK       CONTAINS  1         RECORDS.
     COPY        HSHOTBL   OF        XFDLIB
     JOINING     STB       AS        PREFIX.
 FD  HMEIMS
     LABEL       RECORD    IS        STANDARD
     BLOCK       CONTAINS  1         RECORDS.
     COPY        HMEIMS    OF        XFDLIB
     JOINING     MEI       AS        PREFIX.
 FD  HJYOKEN
     LABEL       RECORD    IS        STANDARD
     BLOCK       CONTAINS  1         RECORDS.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
***## < 担当者マスタ >  ADD 2008/07/30
 FD  HTANMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTANMS    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
***## < 商品変換テーブル履歴ファイル >  ADD 2008/07/30
 FD  HSHOTBR
     LABEL       RECORD    IS        STANDARD.
     COPY        HSHOTBR   OF        XFDLIB
     JOINING     STR       AS        PREFIX.
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FM041     OF        XMDLIB.
 01  FM040R.
*****03  FILLER            PIC X(54).
***##03  FILLER            PIC X(57).
     03  FILLER            PIC X(79).
     03  FM040TBL.
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
*        05  FM040TBLG     PIC X(150) OCCURS  7.
*******##05  FM040TBLG     PIC X(193) OCCURS  7.
         05  FM040TBLG     PIC X(240) OCCURS  7.
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*********************************************************
 WORKING-STORAGE           SECTION.
*********************************************************
* ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT           PIC X(08).
     03  DSP-GRP           PIC X(08).
     03  DSP-PRO           PIC X(02).
     03  DSP-FNC           PIC X(04).
 01  ST-FILE.
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  MEI-ST            PIC X(02).
     03  MEI-ST1           PIC X(04).
     03  STB-ST            PIC X(02).
     03  STB-ST1           PIC X(04).
     03  JYO-ST            PIC X(02).
     03  JYO-ST1           PIC X(04).
     03  TAN-ST            PIC X(02).
     03  TAN-ST1           PIC X(04).
     03  STR-ST            PIC X(02).
     03  STR-ST1           PIC X(04).
     03  DSP-ST            PIC X(02).
     03  DSP-ST1           PIC X(04).
* ﾜｰｸ ｴﾘｱ
 01  IN-DATA               PIC X(01).
*01  SYS-DATE              PIC 9(06).
*01  WK-DATE               PIC 9(08).
*01  WK-DATER    REDEFINES WK-DATE.
*    03  WK-YY             PIC 9(04).
*    03  WK-MM             PIC 9(02).
*    03  WK-DD             PIC 9(02).
*
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
*
 01  WK-AREA.
     03  END-FLG           PIC 9(01) VALUE     ZERO.
     03  IXB               PIC 9(02) VALUE     ZERO.
     03  IXA               PIC 9(02) VALUE     ZERO.
     03  WK-IXA            PIC 9(02) VALUE     ZERO.
     03  WK-WRDATE         PIC 9(08) VALUE     ZERO.
     03  WK-STB-F11        PIC 9(05) VALUE     ZERO.
     03  DAB-SW            PIC 9(01) VALUE     ZERO.
     03  WK-SHOGRP.
         05  WK-SHOGRPTBL  OCCURS  7.
             07  WK-SHO1   PIC X(08).
             07  WK-SHO2   PIC X(05).
             07  WK-SHO3   PIC X(02).
             07  WK-SHO4   PIC X(01).
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
     03  WK-TNAGRP.
         05  WK-TNAGRPTBL  OCCURS  7.
*            07  WK-TNA1   PIC X(03).
*            07  WK-TNA2   PIC X(01).
*            07  WK-TNA3   PIC X(02).
             07  WK-TNA1   PIC X(01).
             07  WK-TNA2   PIC X(03).
             07  WK-TNA3   PIC X(02).
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
***##
 01  WK-STB-AREC           PIC  X(85).
 01  WK-DEL-SW             PIC  9(01) VALUE 0.
******************
*画面グループ編集*
******************
 01  WK-GRP.
     03  WK-GRP1           PIC X(04) VALUE     SPACE.
     03  WK-GRP2           PIC X(02) VALUE     SPACE.
 01  GRP-FIELD             PIC X(14) VALUE
                           "  020304050607".
 01  GRP-FIELD-R           REDEFINES GRP-FIELD.
     03  WK-GRP2R          PIC X(02) OCCURS    7.
****************
*ファイルエラー*
****************
 01  FILE-ERR.
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタ異常".
     03  STB-ERR           PIC N(15) VALUE
                        NC"商品変換マスタ異常".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称マスタ異常".
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件ファイル異常".
     03  TAN-ERR           PIC N(15) VALUE
                        NC"担当者マスタ異常".
     03  STR-ERR           PIC N(15) VALUE
                        NC"商品変換テーブル履歴Ｆ異常".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイル異常".
**************
*ＭＳＧガイド*
**************
 01  MSG-AREA.
     03  MSG01             PIC N(15) VALUE
                        NC"処理区分が違います".
     03  MSG02             PIC N(15) VALUE
                        NC"取引先マスタに未登録".
     03  MSG03             PIC N(15) VALUE
                        NC"商品変換マスタに未登録".
     03  MSG04             PIC N(15) VALUE
                        NC"商品名称マスタに未登録".
     03  MSG05             PIC N(15) VALUE
                        NC"登録済みです".
     03  MSG06             PIC N(15) VALUE
                        NC"重複しています".
     03  MSG07             PIC N(15) VALUE
                        NC"出荷場所が未登録".
     03  MSG08             PIC N(15) VALUE
                        NC"削除区分が違います".
     03  MSG09             PIC N(15) VALUE
                        NC"量販店商品ＣＤを入力して下さい".
     03  MSG10             PIC N(15) VALUE
                        NC"Ｙで入力して下さい".
     03  MSG11             PIC N(15) VALUE
                        NC"張替区分が違います".
***##  ADD  2008/07/30
     03  MSGW1             PIC N(02) VALUE
                        NC"原価".
****************
*ＰＦキーガイド*
****************
 01  FNC-AREA.
     03  FNC00             PIC N(30) VALUE
         NC"_業務終了".
     03  FNC01             PIC N(30) VALUE
         NC"_取消".
     03  FNC02             PIC N(30) VALUE
         NC"_取消　_項目戻".
     03  FNC03             PIC N(30) VALUE
         NC"_取消　_明細終了　_項目戻　_行Ｕ　_行Ｄ".
     03  FNC04             PIC N(30) VALUE
         NC"_取消　_明細終了　_項目戻　_行Ｕ".
     03  FNC05             PIC N(30) VALUE
*        NC"_取消　_明細終了　_項目戻　_行Ｄ".
         NC"_取消　_明細終了　_行Ｄ".
     03  FNC06             PIC N(30) VALUE
         NC"_取消　_再入力".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*ヘッダ情報格納領域
     COPY   HSHOTBL  OF XFDLIB  JOINING   TBL  AS   PREFIX.
***##  ADD  2008/07/30
 LINKAGE                   SECTION.
 01  LNK-M-TAN             PIC X(02).
 01  LNK-M-BUMON           PIC X(04).
 01  LNK-M-NYURYOKUHI      PIC 9(08).
 01  LNK-M-TIME            PIC 9(04).
**********************************************************
 PROCEDURE         DIVISION   USING  LNK-M-TAN  LNK-M-BUMON
                              LNK-M-NYURYOKUHI  LNK-M-TIME.
**********************************************************
 DECLARATIVES.
 STB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     STB-ST    UPON      STA.
     DISPLAY     STB-ERR   UPON      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ST    UPON      STA.
     DISPLAY     TOK-ERR   UPON      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ST    UPON      STA.
     DISPLAY     JYO-ERR   UPON      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ST    UPON      STA.
     DISPLAY     MEI-ERR   UPON      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ST    UPON      STA.
     DISPLAY     TAN-ERR   UPON      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 STR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBR.
     DISPLAY     STR-ST    UPON      STA.
     DISPLAY     STR-ERR   UPON      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPF.
     DISPLAY     DSP-ST    UPON      STA.
     DISPLAY     DSP-ERR   UPON      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA   FROM      STA.
     STOP        RUN.
 END DECLARATIVES.
**********************************************************
*                      Ｐ Ｒ Ｏ Ｃ                       *
**********************************************************
 PROC-SEC                  SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC.
     PERFORM     END-SEC.
*チェックリスト自動出力の為
     MOVE    SYS-DATE      TO        LNK-M-NYURYOKUHI.
     MOVE    WK-TIME(1:4)  TO        LNK-M-TIME.
 PROC-EXIT.
     STOP        RUN.
**********************************************************
*                      Ｍ Ａ Ｉ Ｎ                       *
**********************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSPINIT-SEC.
**************
*処理区分入力*
**************
 MAIN-010.
     MOVE        FNC00     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-011.
     MOVE       "KUBUN"    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F005"
         GO                TO        MAIN-EXIT
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-011
     END-EVALUATE.
     MOVE        SPACE     TO        ERRMSG.
     PERFORM     DSPMSG-SEC.
 MAIN-012.
     IF  (KUBUN = 1  OR  2)
         MOVE   "M"        TO        EDIT-OPTION   OF    KUBUN
         MOVE   "KUBUN"    TO        DSP-GRP
         PERFORM DSPWT-SEC
       ELSE
         MOVE    MSG01     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE   "R"        TO        EDIT-OPTION   OF    KUBUN
         MOVE   "KUBUN"    TO        DSP-GRP
         PERFORM DSPWT-SEC
         GO                TO        MAIN-011.
**************
*取引先CD入力*
**************
 MAIN-020.
     MOVE        FNC01     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
     PERFORM     BDCLR-SEC.
 MAIN-021.
     MOVE       "TORICD"   TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-SEC
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-021
     END-EVALUATE.
     MOVE        SPACE     TO        ERRMSG.
     PERFORM     DSPMSG-SEC.
 MAIN-022.
     MOVE        TORICD    TO        TOK-F01.
     READ  HTOKMS
       INVALID
         MOVE    SPACE     TO        TORINM
         MOVE    MSG02     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE   "R"        TO        EDIT-OPTION   OF    TORICD
         MOVE   "GRP001"   TO        DSP-GRP
         PERFORM DSPWT-SEC
         GO                TO        MAIN-021
       NOT INVALID
         MOVE    TOK-F02   TO        TORINM
         MOVE   "M"        TO        EDIT-OPTION   OF    TORICD
         MOVE   "GRP001"   TO        DSP-GRP
         PERFORM DSPWT-SEC
         MOVE    1         TO        IXA.
******************
*処理区分チェック*
******************
 MAIN-030.
     IF  (KUBUN = 1)
         MOVE    1         TO        IXA
         GO                TO        MAIN-060.
************************
*修正時量販店商品CD入力*
***********************
 MAIN-040.
     MOVE        FNC01     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
     PERFORM     BDCLR-SEC.
 MAIN-041.
     MOVE       "RYOU  "   TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-041
     END-EVALUATE.
 MAIN-042.
     MOVE        TORICD    TO        STB-F01.
     MOVE        RYOU(01)  TO        STB-F02.
     START       HSHOTBL   KEY IS NOT LESS THAN STB-F01 STB-F02
       INVALID
         MOVE    MSG03     TO        ERRMSG
         PERFORM DSPMSG-SEC
         GO                TO        MAIN-041.
     READ        HSHOTBL   NEXT      AT        END
                 MOVE      MSG03     TO        ERRMSG
                 PERFORM   DSPMSG-SEC
                 GO        TO        MAIN-041.
     IF  (TORICD NOT = STB-F01)
         MOVE    MSG03     TO        ERRMSG
         PERFORM DSPMSG-SEC
         GO                TO        MAIN-041.
 MAIN-043.
     MOVE        ZERO      TO        IXA.
     MOVE        SPACE     TO        FM040TBL.
     PERFORM     DATADSP-SEC.
     MOVE       "BODY"     TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
     MOVE        1         TO        IXA.
********************
*修正時削除区分入力*
********************
 MAIN-050.
     EVALUATE  IXA
       WHEN
         1
         MOVE    FNC05     TO        PFNAME
       WHEN
         WK-IXA
         MOVE    FNC04     TO        PFNAME
       WHEN
         OTHER
         MOVE    FNC03     TO        PFNAME
     END-EVALUATE.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-051.
     MOVE       "SACD"     TO        WK-GRP1.
     MOVE        WK-GRP2R(IXA) TO    WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
         WHEN
          "F004"
           GO              TO        MAIN-040
         WHEN
          "F005"
           GO              TO        MAIN-120
         WHEN
          "F006"
           IF  (IXA  = 1)
               GO          TO        MAIN-050
             ELSE
               SUBTRACT    1         FROM      IXA
               IF  (SACD(IXA) = "9")
                   GO      TO        MAIN-050
                 ELSE
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
*                  GO      TO        MAIN-110
*******************GO      TO        MAIN-160
*# 2008/08/25 NAV T.T
                   GO      TO        MAIN-310
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
               END-IF
           END-IF
         WHEN
          "F007"
           IF  (IXA  > 1)
               SUBTRACT    1         FROM      IXA
           END-IF
           GO              TO        MAIN-050
         WHEN
          "F008"
           IF  (IXA  < WK-IXA)
               ADD         1         TO        IXA
           END-IF
           GO              TO        MAIN-050
         WHEN
          "E000"
           CONTINUE
         WHEN
           OTHER
           GO              TO        MAIN-051
     END-EVALUATE.
 MAIN-052.
     EVALUATE    SACD(IXA)
       WHEN
        "9"
         MOVE   "M"        TO        EDIT-OPTION   OF    SACD(IXA)
         MOVE   "SACD"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
*
         MOVE NC"削除"     TO        SANM(IXA)
         MOVE   "SANM"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
*
         MOVE    SPACE     TO        ERRMSG
         PERFORM DSPMSG-SEC
*
         ADD     1         TO        IXA
         IF  (IXA > WK-IXA)
             GO            TO        MAIN-120
         ELSE
             GO            TO        MAIN-050
         END-IF
       WHEN
         SPACE
         MOVE   "M"        TO        EDIT-OPTION   OF    SACD(IXA)
         MOVE   "SACD"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
*
         MOVE    SPACE     TO        SANM(IXA)
         MOVE   "SANM"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
*
         MOVE    SPACE     TO        ERRMSG
         PERFORM DSPMSG-SEC
*
         GO                TO        MAIN-070
       WHEN
         OTHER
         MOVE   "R"        TO        EDIT-OPTION   OF    SACD(IXA)
         MOVE   "SACD"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
*
         MOVE    MSG08     TO        ERRMSG
         PERFORM DSPMSG-SEC
*
         GO                TO        MAIN-051
     END-EVALUATE.
************************
*登録時量販店商品CD入力*
***********************
 MAIN-060.
     EVALUATE  IXA
       WHEN
         1
         MOVE    FNC05     TO        PFNAME
       WHEN
         7
         MOVE    FNC04     TO        PFNAME
       WHEN
         OTHER
         MOVE    FNC03     TO        PFNAME
     END-EVALUATE.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-061.
     MOVE       "RYOU"     TO        WK-GRP1.
     MOVE        WK-GRP2R(IXA) TO    WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
         WHEN
          "F004"
           GO              TO        MAIN-020
         WHEN
          "F005"
           GO              TO        MAIN-120
         WHEN
          "F006"
           IF  (IXA  = 1)
               GO          TO        MAIN-061
             ELSE
               SUBTRACT    1         FROM      IXA
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
*              GO          TO        MAIN-110
*##############GO          TO        MAIN-160
               GO          TO        MAIN-310
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
           END-IF
         WHEN
          "F007"
           IF  (IXA  > 1)
               SUBTRACT    1         FROM      IXA
           END-IF
           GO              TO        MAIN-060
         WHEN
          "F008"
           IF  (IXA  < 7)
               ADD         1         TO        IXA
           END-IF
           GO              TO        MAIN-060
         WHEN
          "E000"
           CONTINUE
         WHEN
           OTHER
           GO              TO        MAIN-061
     END-EVALUATE.
 MAIN-062.
*空白チェック
     IF  (RYOU(IXA) = SPACE)
         MOVE    MSG09     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE   "R"        TO        EDIT-OPTION   OF    RYOU(IXA)
         MOVE   "RYOU"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
         GO                TO        MAIN-061.
*存在チェック
     MOVE        TORICD    TO        STB-F01
     MOVE        RYOU(IXA) TO        STB-F02
     READ    HSHOTBL
       INVALID
         CONTINUE
       NOT INVALID
         MOVE    MSG05     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE   "R"        TO        EDIT-OPTION   OF    RYOU(IXA)
         MOVE   "RYOU"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
         MOVE    SPACE     TO        RYOU(IXA)
         GO                TO        MAIN-061.
*ダブリチェック
     MOVE        ZERO      TO        DAB-SW.
     PERFORM     VARYING   IXB       FROM    1  BY  1
                 UNTIL     IXB > 7   OR  DAB-SW = 1
         IF  (RYOU(IXB) NOT = SPACE) AND  (IXA NOT = IXB)
             IF  (RYOU(IXA) = RYOU(IXB))
                 MOVE          1         TO        DAB-SW
             END-IF
         END-IF
     END-PERFORM.
     IF  (DAB-SW = 1)
         MOVE    MSG06     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE   "R"        TO        EDIT-OPTION   OF    RYOU(IXA)
         MOVE   "RYOU"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
         MOVE    SPACE     TO        RYOU(IXA)
         GO                TO        MAIN-061
       ELSE
         MOVE    SPACE     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE   "M"        TO        EDIT-OPTION   OF    RYOU(IXA)
         MOVE   "RYOU"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC.
*
**************
*ここから共通*
**************
****************
*自社商品CD入力*
****************
 MAIN-070.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-071.
     MOVE       "SHOH"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
         IF  (KUBUN = 2)
             GO            TO        MAIN-050
           ELSE
             GO            TO        MAIN-060
         END-IF
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-071
     END-EVALUATE.
 MAIN-072.
     MOVE        SHO1(IXA) TO        MEI-F011.
     MOVE        SHO2(IXA) TO        MEI-F0121.
     MOVE        SHO3(IXA) TO        MEI-F0122.
     MOVE        SHO4(IXA) TO        MEI-F0123.
     READ        HMEIMS
       INVALID
         MOVE    MSG04     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE    SPACE     TO        SHON(IXA)
         MOVE   "R"        TO        EDIT-OPTION   OF    SHO1(IXA)
         MOVE   "R"        TO        EDIT-OPTION   OF    SHO2(IXA)
         MOVE   "R"        TO        EDIT-OPTION   OF    SHO3(IXA)
         MOVE   "R"        TO        EDIT-OPTION   OF    SHO4(IXA)
         MOVE   "SHOH"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
         MOVE    SPACE     TO        SHO1(IXA) SHO2(IXA)
                                     SHO3(IXA) SHO4(IXA)
                                     WK-SHO1(IXA) WK-SHO2(IXA)
                                     WK-SHO3(IXA) WK-SHO4(IXA)
         GO                TO        MAIN-071
       NOT INVALID
         MOVE    MEI-F021  TO        SHON(IXA)
         IF  (SHO1(IXA) NOT = WK-SHO1(IXA)) OR
             (SHO2(IXA) NOT = WK-SHO2(IXA)) OR
             (SHO3(IXA) NOT = WK-SHO3(IXA)) OR
             (SHO4(IXA) NOT = WK-SHO4(IXA))
             MOVE MEI-F042 TO        GENT(IXA)
             MOVE MEI-F043 TO        URIT(IXA)
         END-IF
         MOVE   "M"        TO        EDIT-OPTION   OF    SHO1(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SHO2(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SHO3(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SHO4(IXA)
         MOVE   "SHOG"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
         MOVE    SHO1(IXA) TO        WK-SHO1(IXA).
         MOVE    SHO2(IXA) TO        WK-SHO2(IXA).
         MOVE    SHO3(IXA) TO        WK-SHO3(IXA).
         MOVE    SHO4(IXA) TO        WK-SHO4(IXA).
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
************
*_番入力　*
************
 MAIN-140.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-141.
     MOVE       "TANA"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
         GO                TO        MAIN-070
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-141
     END-EVALUATE.
 MAIN-142.
     MOVE       "TANA"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
****************
*出荷場所CD入力*
****************
 MAIN-080.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-081.
     MOVE       "SYUK"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
*        GO                TO        MAIN-070
         GO                TO        MAIN-140
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-081
     END-EVALUATE.
 MAIN-082.
     MOVE        20        TO        JYO-F01.
     MOVE        SYUK(IXA) TO        JYO-F02.
     READ        HJYOKEN
       INVALID
         MOVE    MSG07     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE   "R"        TO        EDIT-OPTION   OF    SYUK(IXA)
         MOVE   "SYUK"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC
         MOVE    SPACE     TO        SYUK(IXA)
         GO                TO        MAIN-081
       NOT INVALID
         MOVE    SPACE     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE   "M"        TO        EDIT-OPTION   OF    SYUK(IXA)
         MOVE   "SYUK"     TO        WK-GRP1
         MOVE    WK-GRP2R(IXA) TO    WK-GRP2
         MOVE    WK-GRP    TO        DSP-GRP
         PERFORM DSPWT-SEC.
************
*分類CD入力*
************
 MAIN-090.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-091.
     MOVE       "BUNC"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
         GO                TO        MAIN-080
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-091
     END-EVALUATE.
 MAIN-092.
     MOVE       "BUNC"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
************
*原単価入力*
************
 MAIN-100.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-101.
     MOVE       "GENT"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
         GO                TO        MAIN-090
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-101
     END-EVALUATE.
 MAIN-102.
     MOVE       "GENT"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
************
*売単価入力*
************
 MAIN-110.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-111.
     MOVE       "URIT"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
         GO                TO        MAIN-100
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-111
     END-EVALUATE.
 MAIN-112.
     MOVE       "URIT"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
***##  ADD  2008/07/30
************
*仕入単価入力*
************
 MAIN-210.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-211.
     MOVE       "SIIT"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
         GO                TO        MAIN-110
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-211
     END-EVALUATE.
 MAIN-212.
     MOVE       "SIIT"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
***##  ADD  2008/07/31
*M   IF  SACD(IXA)  =  "9"
*M       GO  TO   MAIN-NEW-010S.
***##  仕入単価チェック
     IF  SIIT(IXA)  <=  GENT(IXA)  AND
         GENT(IXA)  <=  URIT(IXA)
         MOVE  SPACE   TO  SANM(IXA)
     ELSE
         MOVE  MSGW1   TO  SANM(IXA)
     END-IF.
     MOVE       "SANM"     TO       WK-GRP1.
     MOVE       WK-GRP2R(IXA)  TO   WK-GRP2.
     MOVE       WK-GRP     TO       DSP-GRP.
     PERFORM    DSPWT-SEC.
*    GO  TO     MAIN-NEW-010S.
************
*張替入力*
************
 MAIN-310.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-311.
     MOVE       "HRKE"    TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
         GO                TO        MAIN-210
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-311
     END-EVALUATE.
 MAIN-312.
     MOVE       "HRKE"    TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
*2013/02/18↓
*****IF  (HRKE(IXA) = SPACE OR  "1")
     IF  (HRKE(IXA) = SPACE OR  "1"  OR  "2")
*2013/02/18↑
         MOVE    SPACE     TO        ERRMSG
         PERFORM     DSPMSG-SEC
         MOVE   "M"        TO        EDIT-OPTION   OF  HRKE(IXA)
         PERFORM     DSPWT-SEC
         GO  TO     MAIN-NEW-010S
     ELSE
         MOVE    MSG11     TO        ERRMSG
         PERFORM     DSPMSG-SEC
         MOVE   "R"        TO        EDIT-OPTION   OF  HRKE(IXA)
         PERFORM     DSPWT-SEC
         GO                TO        MAIN-311
     END-IF.
***##  DELETE STEP  208/07/30  ************
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
**************
*在庫期間入力*
**************
 MAIN-150.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-151.
     MOVE       "ZKIK"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F006"
         GO                TO        MAIN-110
       WHEN
        "E000"
         CONTINUE
       WHEN
         OTHER
         GO                TO        MAIN-151
     END-EVALUATE.
 MAIN-152.
     MOVE       "ZKIK"     TO        WK-GRP1.
     MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
     MOVE        WK-GRP    TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
**************
*安全係数入力*
**************
 MAIN-160.
     MOVE        FNC02     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-161.
*#   MOVE       "KEIS"     TO        WK-GRP1.
*#   MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
*#   MOVE        WK-GRP    TO        DSP-GRP.
*#   PERFORM     DSPRD-SEC.
*#   EVALUATE    DSP-FNC
*#     WHEN
*#      "F004"
*#       GO                TO        MAIN-020
*#     WHEN
*#      "F006"
*#       GO                TO        MAIN-150
*#     WHEN
*#      "E000"
*#       CONTINUE
*#     WHEN
*#       OTHER
*#       GO                TO        MAIN-161
*#   END-EVALUATE.
 MAIN-162.
*#   MOVE       "KEIS"     TO        WK-GRP1.
*#   MOVE    WK-GRP2R(IXA) TO        WK-GRP2.
*#   MOVE        WK-GRP    TO        DSP-GRP.
*#   PERFORM     DSPWT-SEC.
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
***## 2008/07/30
 MAIN-NEW-010S.
     ADD         1         TO        IXA.
     IF  (KUBUN = 1)
         IF  (IXA > 7)
             CONTINUE
           ELSE
             GO            TO        MAIN-060
         END-IF
       ELSE
         IF  (IXA > WK-IXA)
             CONTINUE
           ELSE
             GO            TO        MAIN-050
         END-IF
     END-IF.
     MOVE        SPACE     TO        ERRMSG.
     PERFORM     DSPMSG-SEC.
**********
*確認入力*
**********
 MAIN-120.
     MOVE        FNC06     TO        PFNAME.
     MOVE       "PFNAME"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
     MOVE        SPACE     TO        ERRMSG.
     MOVE       "ERRMSG"   TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
*
     MOVE       "Y"        TO        KAKU.
     MOVE       "KAKU"     TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 MAIN-121.
     MOVE       "KAKU"     TO        DSP-GRP.
     PERFORM     DSPRD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
         GO                TO        MAIN-020
       WHEN
        "F009"
         MOVE    SPACE     TO        ERRMSG
         PERFORM DSPMSG-SEC
         MOVE    1         TO        IXA
         IF  (KUBUN = 1)
             GO            TO        MAIN-060
           ELSE
             GO            TO        MAIN-050
         END-IF
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           GO              TO        MAIN-121
     END-EVALUATE.
 MAIN-122.
     IF  (KAKU = "Y")
         MOVE    SPACE     TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSPWT-SEC
       ELSE
         MOVE    MSG10     TO        ERRMSG
         MOVE   "ERRMSG"   TO        DSP-GRP
         PERFORM DSPWT-SEC
         GO                TO        MAIN-121
     END-IF.
**************
*レコード更新*
**************
 MAIN-130.
     IF  (KUBUN = 2)
         PERFORM VARYING   IXA FROM 1 BY 1
                           UNTIL     IXA >  7
             PERFORM       DELET-SEC
             PERFORM       TUIKA-SEC
         END-PERFORM
       ELSE
         PERFORM VARYING   IXA FROM 1 BY 1
                           UNTIL     IXA >  7
             PERFORM       TUIKA-SEC
         END-PERFORM
     END-IF.
     PERFORM     BDCLR-SEC.
     GO                    TO        MAIN-030.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                    修正データ表示  ｼｭｳｾｲﾓｰﾄﾞ           *
**********************************************************
 DATADSP-SEC               SECTION.
     ADD         1         TO        IXA.
     IF  (IXA > 7)
         MOVE    7         TO        WK-IXA
         GO                TO        DATADSP-EXIT.
*
     MOVE        STB-F02   TO        RYOU(IXA).
     MOVE        STB-F031  TO        SHO1(IXA) WK-SHO1(IXA).
     MOVE        STB-F0321 TO        SHO2(IXA) WK-SHO2(IXA).
     MOVE        STB-F0322 TO        SHO3(IXA) WK-SHO3(IXA).
     MOVE        STB-F0323 TO        SHO4(IXA) WK-SHO4(IXA).
     MOVE        STB-F04   TO        SYUK(IXA).
     MOVE        STB-F05   TO        GENT(IXA).
     MOVE        STB-F06   TO        URIT(IXA).
     MOVE        STB-F07   TO        BUNC(IXA).
     MOVE        STB-F10   TO        HRKE(IXA).
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
*    MOVE        STB-F081  TO        TNA1(IXA) WK-TNA1(IXA).
*    MOVE        STB-F082  TO        TNA2(IXA) WK-TNA2(IXA).
*    MOVE        STB-F083  TO        TNA3(IXA) WK-TNA3(IXA).
     MOVE        STB-F08(1:1)  TO        TNA1(IXA) WK-TNA1(IXA).
     MOVE        STB-F08(2:3)  TO        TNA2(IXA) WK-TNA2(IXA).
     MOVE        STB-F08(5:2)  TO        TNA3(IXA) WK-TNA3(IXA).
***##     MOVE        STB-F09   TO        AKEI(IXA).
***##     MOVE        STB-F10   TO        ZAIK(IXA).
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
     MOVE        STB-F09   TO        SIIT(IXA).
*
     MOVE        STB-F031  TO        MEI-F011.
     MOVE        STB-F0321 TO        MEI-F0121.
     MOVE        STB-F0322 TO        MEI-F0122.
     MOVE        STB-F0323 TO        MEI-F0123.
     READ        HMEIMS
       INVALID
         MOVE ALL  NC"＊"  TO        SHON(IXA)
       NOT INVALID
         MOVE    MEI-F021  TO        SHON(IXA).
*
     READ        HSHOTBL   NEXT      AT        END
                 MOVE      IXA       TO        WK-IXA
                 GO        TO        DATADSP-EXIT.
     IF  (TORICD NOT = STB-F01)
         MOVE    IXA       TO        WK-IXA
       ELSE
         GO                TO        DATADSP-SEC.
 DATADSP-EXIT.
     EXIT.
**********************************************************
*                 修正データ削除  ｼｭｳｾｲﾓｰﾄﾞ              *
**********************************************************
 DELET-SEC                 SECTION.
***##
     MOVE     0    TO   WK-DEL-SW.
     IF  (SACD(IXA) = SPACE)
         IF  (RYOU(IXA) = SPACE) OR
             (SHO1(IXA) = SPACE) OR
             (SYUK(IXA) = SPACE)
             GO            TO        DELET-EXIT.
     MOVE        TORICD    TO        STB-F01.
     MOVE        RYOU(IXA) TO        STB-F02.
     READ    HSHOTBL
       INVALID
         CONTINUE
       NOT  INVALID
***## ADD  2008/07/30
         MOVE    STB-REC   TO        WK-STB-AREC  TBL-REC
         MOVE    1         TO        WK-DEL-SW
***##
         MOVE    STB-F98   TO        WK-WRDATE
         MOVE    STB-F11   TO        WK-STB-F11
         DELETE  HSHOTBL
         END-DELETE
     END-READ.
 DELET-EXIT.
     EXIT.
**********************************************************
*                    データ作成  共通                    *
**********************************************************
 TUIKA-SEC                 SECTION.
     MOVE        SPACE     TO        STB-REC.
     IF  (RYOU(IXA) NOT = SPACE) AND
         (SHO1(IXA) NOT = SPACE) AND
         (SYUK(IXA) NOT = SPACE) AND
         (SACD(IXA)     = SPACE)
         MOVE    TORICD    TO        STB-F01
         MOVE    RYOU(IXA) TO        STB-F02
         MOVE    SHO1(IXA) TO        STB-F031
         MOVE    SHO2(IXA) TO        STB-F0321
         MOVE    SHO3(IXA) TO        STB-F0322
         MOVE    SHO4(IXA) TO        STB-F0323
         MOVE    SYUK(IXA) TO        STB-F04
         MOVE    GENT(IXA) TO        STB-F05
         MOVE    URIT(IXA) TO        STB-F06
         MOVE    BUNC(IXA) TO        STB-F07
         MOVE    HRKE(IXA) TO       STB-F10
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
*        MOVE    TNA1(IXA) TO        STB-F081
*        MOVE    TNA2(IXA) TO        STB-F082
*        MOVE    TNA3(IXA) TO        STB-F083
         MOVE    TNA1(IXA) TO        STB-F08(1:1)
         MOVE    TNA2(IXA) TO        STB-F08(2:3)
         MOVE    TNA3(IXA) TO        STB-F08(5:2)
***##         MOVE    AKEI(IXA) TO        STB-F09
***##         MOVE    ZAIK(IXA) TO        STB-F10
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
         MOVE    SIIT(IXA) TO        STB-F09
*
         IF  (KUBUN = 1)
             MOVE SYS-DATE  TO       STB-F98
             MOVE  LNK-M-TAN  TO     STB-F13  STB-F14
           ELSE
             MOVE WK-WRDATE TO       STB-F98
             MOVE WK-STB-F11 TO      STB-F11
             MOVE LNK-M-TAN  TO      STB-F14
*************#2008/08/29 内部統制（登録担当者ＣＤをセット）
             MOVE TBL-F13    TO      STB-F13
         END-IF
         MOVE    SYS-DATE   TO       STB-F99
*********MOVE    ZERO       TO       STB-F11
***##  ADD  2008/07/30
         PERFORM  HSHOTBR-WRITE-SEC
***
         WRITE   STB-REC
         END-WRITE
*****削除の場合でも更新する。
     ELSE
         IF  (SACD(IXA)     =  "9")
              PERFORM  HSHOTBD-WRITE-SEC
         END-IF
     END-IF.
 TUIKA-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     OPEN        I-O       DSPF
                           HSHOTBL
                 INPUT     HTOKMS
                           HMEIMS
                           HJYOKEN.
     OPEN        INPUT     HTANMS   I-O  HSHOTBR.
     MOVE        SPACE     TO        WK-SHOGRP.
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
     MOVE        SPACE     TO        WK-TNAGRP.
*   93.04.13  SYUUSEI  START  H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*    ACCEPT      SYS-DATE  FROM      DATE.
**   MOVE        SYS-DATE  TO        WK-DATE.
*    IF  (WK-YY > 89)
*        ADD     1900      TO        WK-YY
*      ELSE
*        ADD     2000      TO        WK-YY.
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
*
 INIT-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     CLOSE       DSPF      HMEIMS    HTOKMS    HSHOTBL   HJYOKEN.
     CLOSE       HTANMS    HSHOTBR.
*チェックリスト自動出力の為
     MOVE    SYS-DATE      TO        LNK-M-NYURYOKUHI.
     MOVE    WK-TIME(1:4)  TO        LNK-M-TIME.
*
 END-EXIT.
     EXIT.
**********************************************************
*                     担当者マスタ読込み                 *
**********************************************************
 HTANMS-READ-SEC             SECTION.
     MOVE   LNK-M-TAN       TO       TAN-F02.
     MOVE   LNK-M-BUMON     TO       TAN-F01.
     READ   HTANMS
            INVALID  KEY
            MOVE            SPACE    TO  TAN-F03
     END-READ.
 HTANMS-READ-EXIT.
     EXIT.
**********************************************************
*                     履歴ファイル書き出し               *
**********************************************************
 HSHOTBR-WRITE-SEC          SECTION.
     INITIALIZE  STR-REC.
     IF  WK-DEL-SW  =  1
         MOVE    WK-STB-AREC  TO  STR-REC(16:85)
         MOVE    0      TO  WK-DEL-SW
     END-IF.
     MOVE   STB-REC     TO  STR-REC(101:85).
     MOVE   SYS-DATE    TO  STR-F01.
     MOVE   WK-TIME(1:4)  TO  STR-F02.
     MOVE   LNK-M-TAN   TO  STR-F03.
     MOVE   KUBUN       TO  STR-F04.
     IF     WK-STB-AREC(1:71) NOT = STB-REC(1:71)
*****DISPLAY "WK-STB-AREC = "  WK-STB-AREC(1:71)  UPON CONS
*****DISPLAY "STB-REC     = "  STB-REC(1:71)      UPON CONS
*****DISPLAY "F01 = " TBL-F01 " : " STB-F01  UPON CONS
*****DISPLAY "F02 = " TBL-F02 " : " STB-F02  UPON CONS
*****DISPLAY "F03 = " TBL-F03 " : " STB-F03  UPON CONS
*****DISPLAY "F04 = " TBL-F04 " : " STB-F04  UPON CONS
*****DISPLAY "F05 = " TBL-F05 " : " STB-F05  UPON CONS
*****DISPLAY "F06 = " TBL-F06 " : " STB-F06  UPON CONS
*****DISPLAY "F07 = " TBL-F07 " : " STB-F07  UPON CONS
*****DISPLAY "F08 = " TBL-F08 " : " STB-F08  UPON CONS
*****DISPLAY "F09 = " TBL-F09 " : " STB-F09  UPON CONS
*****DISPLAY "F10 = " TBL-F10 " : " STB-F10  UPON CONS
*****DISPLAY "F11 = " TBL-F11 " : " STB-F11  UPON CONS
*****DISPLAY "F12 = " TBL-F12 " : " STB-F12  UPON CONS
*****DISPLAY "F13 = " TBL-F13 " : " STB-F13  UPON CONS
*****DISPLAY "F14 = " TBL-F14 " : " STB-F14  UPON CONS
*****DISPLAY "F98 = " TBL-F98 " : " STB-F98  UPON CONS
*****DISPLAY "F99 = " TBL-F99 " : " STB-F99  UPON CONS
            WRITE  STR-REC
     END-IF.
 HSHOTBR-WRITE-EXIT.
     EXIT.
**********************************************************
*    履歴ファイル更新（削除の場合）
**********************************************************
 HSHOTBD-WRITE-SEC          SECTION.
     INITIALIZE  STR-REC.
     MOVE   WK-STB-AREC TO  STR-REC(16:85).
     MOVE   TORICD      TO  STR-FB01.
     MOVE   SYS-DATE    TO  STR-F01.
     MOVE   WK-TIME(1:4)  TO  STR-F02.
     MOVE   LNK-M-TAN   TO  STR-F03.
     MOVE   "3"         TO  STR-F04.
     WRITE  STR-REC.
 HSHOTBD-WRITE-EXIT.
     EXIT.
**********************************************************
*                     画面初期表示                       *
**********************************************************
 DSPINIT-SEC               SECTION.
     MOVE        SPACE     TO        FM041.
     MOVE       "ALLF"     TO        DSP-GRP.
     MOVE       "FM041"    TO        DSP-FMT.
     MOVE       "CL"       TO        DSP-PRO.
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
     PERFORM     VARYING   IXA       FROM     1  BY  1
                 UNTIL     IXA       >        7
         MOVE    IXA       TO        LNO(IXA)
     END-PERFORM.
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
     MOVE        HEN-DATE  TO        SDATE.
     MOVE        HEN-TIME  TO        STIME.
***##  ADD  2008/07/30
     PERFORM     HTANMS-READ-SEC.
     MOVE        TAN-F02   TO        TANCD.
     MOVE        TAN-F03   TO        TANNM.
***##
     PERFORM     DSPWT-SEC.
     INITIALIZE  FM041.
 DSPINIT-EXIT.
     EXIT.
**********************************************************
*                     画面項目入力                       *
**********************************************************
 DSPRD-SEC                 SECTION.
     MOVE       "NE"       TO        DSP-PRO.
     READ        DSPF      AT        END
                 GO        TO        DSPRD-EXIT.
 DSPRD-EXIT.
     EXIT.
**********************************************************
*                     画面項目表示                       *
**********************************************************
 DSPWT-SEC              SECTION.
     MOVE        SPACE     TO        DSP-PRO.
     WRITE       FM041.
 DSPWT-EXIT.
     EXIT.
**********************************************************
*                   各メッセージ表示                     *
**********************************************************
 DSPMSG-SEC                SECTION.
     MOVE       "ERRMSG"             TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
 DSPMSG-EXIT.
     EXIT.
**********************************************************
*                   ボディー部クリアー                   *
**********************************************************
 BDCLR-SEC                 SECTION.
     MOVE        SPACE     TO        FM040TBL
                                     KAKU.
     PERFORM     VARYING   IXA       FROM     1  BY  1
                 UNTIL     IXA       >        7
         MOVE   "M"        TO        EDIT-OPTION   OF    SACD(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    RYOU(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SHO1(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SHO2(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SHO3(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SHO4(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SYUK(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    SIIT(IXA)
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
         MOVE   "M"        TO        EDIT-OPTION   OF    TNA1(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    TNA2(IXA)
         MOVE   "M"        TO        EDIT-OPTION   OF    TNA3(IXA)
***##         MOVE   "M"        TO        EDIT-OPTION   OF    ZAIK
***##         MOVE   "M"        TO        EDIT-OPTION   OF    AKEI
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
     END-PERFORM.
     MOVE       "BODY"     TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
     MOVE       "KAKU"     TO        DSP-GRP.
     PERFORM     DSPWT-SEC.
     PERFORM     VARYING   IXA       FROM     1  BY  1
                 UNTIL     IXA       >        7
         MOVE    ZERO      TO        GENT(IXA)  URIT(IXA)
         MOVE    ZERO      TO        SIIT(IXA)
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
***##         MOVE    ZERO      TO        ZAIK(IXA)  AKEI(IXA)
         MOVE    IXA       TO        LNO(IXA)
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
     END-PERFORM.
     MOVE        SPACE     TO        ERRMSG.
     PERFORM     DSPMSG-SEC.
     MOVE        SPACE     TO        WK-SHOGRP.
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
*   93.04.13  SYUUSEI  START  H.SUTO
     MOVE        SPACE     TO        WK-TNAGRP.
*   93.04.13  SYUUSEI  END    H.SUTO
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
 BDCLR-EXIT.
     EXIT.

```
