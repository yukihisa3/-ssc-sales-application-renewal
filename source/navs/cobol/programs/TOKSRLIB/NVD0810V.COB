****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＣＳＶ出力　　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定エラーリストＣＳＶ出力　　*
*    作成日／作成者　　　：　2022/07/28 INOUE                  *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　入荷予定よりエラーデータのみを　　*
*    　　　　　　　　　　　　ＣＳＶ出力する。　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0810V.
*                  流用:NVD0152V
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入荷予定リストＷ　   >>******************************
*    SELECT   DNDTKNL1  ASSIGN  TO   DA-01-S-DNDTKNL1
*                       ORGANIZATION         IS  SEQUENTIAL
*                       ACCESS  MODE         IS  SEQUENTIAL
*                       FILE STATUS          IS  NYY-STATUS.
*ＮＡＶＳ受信データ管理ファイルL1
     SELECT  DNDTKNL1 ASSIGN        TO  DNDTKNL1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  DYNAMIC
                      RECORD KEY    IS  DND1-F01 *> 送受信指示NO
                                        DND1-F07 *> 取込日付
                                        DND1-F08 *> 取込時刻
                                        DND1-F09 *> 伝票番号
                                        DND1-F10 *> 行番号
                      FILE   STATUS IS  DND1-STATUS.
*ＮＡＶＳ受信データ管理ファイルL3
     SELECT  DNDTKNL3 ASSIGN        TO  DNDTKNL3
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  DYNAMIC
                      RECORD KEY    IS  DND3-F07 *> 取込日付
                                        DND3-F08 *> 取込時刻
                                        DND3-F09 *> 伝票番号
                                        DND3-F10 *> 行番号
                      FILE   STATUS IS  DND3-STATUS.
****<< SUB商品名称マスタ　  >>******************************
     SELECT   SUBMEIL7   ASSIGN  TO   DA-01-VI-SUBMEIL7
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  RANDOM
                        RECORD  KEY          IS  MEI-D01
                        FILE STATUS          IS  MEI-STATUS.
****<< 倉庫マスタ　　　　  >>******************************
*    SELECT   ZSOKMS1   ASSIGN  TO   DA-01-VI-ZSOKMS1
*                       ORGANIZATION         IS  INDEXED
*                       ACCESS  MODE         IS  RANDOM
*                       RECORD  KEY          IS  SOK-F01
*                       FILE STATUS          IS  SOK-STATUS.
****<< 条件ファイル　　　  >>******************************
*    SELECT   JYOKEN1   ASSIGN  TO   DA-01-VI-JYOKEN1
*                       ORGANIZATION         IS  INDEXED
*                       ACCESS  MODE         IS  RANDOM
*                       RECORD  KEY          IS  JYO-F01
*                                                JYO-F02
*                       FILE STATUS          IS  JYO-STATUS.
****<< 入荷予定エラーリストＣＳＶ >>****************************
     SELECT   NYYOTEER    ASSIGN  TO   DA-01-S-NYYOTEER
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   CSV-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 入荷予定リストＷ　　　 >>******************************
*FD    DNDTKNL1
*      LABEL    RECORD      IS      STANDARD
*      BLOCK    CONTAINS    6       RECORDS.
*      COPY     DNDTKNL1    OF        XFDLIB
*               JOINING   NYY       PREFIX.
****<< ＮＡＶＳ受信データ管理ファイルL1 >>********************
 FD  DNDTKNL1.
     COPY        DNDTKNL1    OF        XFDLIB
     JOINING     DND1        AS        PREFIX.
****<< ＮＡＶＳ受信データ管理ファイルL3 >>********************
 FD  DNDTKNL3.
     COPY        DNDTKNL3    OF        XFDLIB
     JOINING     DND3        AS        PREFIX.
****<< SUB商品名称マスタ　　　>>******************************
 FD    SUBMEIL7.
       COPY     SUBMEIL7    OF        XFDLIB
                JOINING   MEI       PREFIX.
****<< 倉庫マスタ　　　　　　 >>******************************
*FD    ZSOKMS1.
*      COPY     ZSOKMS1     OF        XFDLIB
*               JOINING   SOK       PREFIX.
****<< 条件ファイル　　　　　 >>******************************
*FD    JYOKEN1.
*      COPY     JYOKEN1     OF        XFDLIB
*               JOINING   JYO       PREFIX.
****<< 入荷予定エラーリストＣＳＶ >>************************
 FD    NYYOTEER.
       COPY     NYYOTEE2   OF       XFDLIB
                JOINING   CSV       PREFIX.
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  ＣＳＶ タイトル行       ****
 COPY   NYYOTEE1   OF        XFDLIB
        JOINING    CSV1      PREFIX.
****  画面制御項目            ****
*01  DSP-CONTROL.
*    02 DSP-PROC             PIC  X(2).
*    02 DSP-GROUP            PIC  X(8).
*    02 DSP-FORMAT           PIC  X(8).
*    02 DSP-STATUS           PIC  X(2).
*    02 DSP-FUNC             PIC  X(4).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 DND1-STATUS          PIC  X(2).
     02 DND3-STATUS          PIC  X(2).
     02 CSV-STATUS           PIC  X(2).
     02 NYY-STATUS           PIC  X(2).
     02 MEI-STATUS           PIC  X(2).
     02 SOK-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
****  カウンタ                ****
 01  DNDTKNF-CNT             PIC  9(8)   VALUE  ZERO.
 01  NYYOTEER-CNT            PIC  9(8)   VALUE  ZERO.
****  フラグ                  ****
 01  DSP-FLG                 PIC  X(01)  VALUE  SPACE.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  INV-FLG                 PIC  X(03)  VALUE  SPACE.
 01  PG-END                  PIC  X(03)  VALUE  SPACE.
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
**** エラーメッセージ         ****
*01  ERR-TAB.
*    02  MSG-ERR1            PIC  N(30)  VALUE
*           NC"無効ＰＦキーです。".
*    02  MSG-ERR2            PIC  N(30)  VALUE
*           NC"開始・終了コードの関係に誤りがあります。".
*    02  PMSG01              PIC N(20) VALUE
*           NC"_取消　_終了".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0810V".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*変換１
 01  HENKAN1                 PIC  9(11).
 01  HENKAN1R          REDEFINES      HENKAN1.
     03  HENKAN1-1           PIC  9(09).
     03  HENKAN1-2           PIC  9(02).
*
 01  HENKAN1-DATA.
*    03  HENKAN1-DATA0       PIC  X(01).
     03  HENKAN1-DATA1       PIC  X(09).
     03  HENKAN1-DATA2       PIC  X(01).
     03  HENKAN1-DATA3       PIC  X(02).
 01  TANKA1-DATA.
     03  TANKA1-DATA1        PIC  9(08).
     03  TANKA1-DATA2        PIC  X(01).
     03  TANKA1-DATA3        PIC  9(02).
*変換２
 01  HENKAN2                 PIC  9(01)V9(02).
 01  HENKAN2R          REDEFINES      HENKAN2.
     03  HENKAN2-1           PIC  9(01).
     03  HENKAN2-2           PIC  9(02).
*
 01  HENKAN2-DATA.
     03  HENKAN2-DATA1       PIC  X(01).
     03  HENKAN2-DATA2       PIC  X(01).
     03  HENKAN2-DATA3       PIC  X(02).
*
 01  WK-KANJI.
     03  WK-KANJI1           PIC  N(15)   VALUE  SPACE.
     03  WK-KANJI2           PIC  N(15)   VALUE  SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*入荷予定レコード展開
     COPY        YTINYKF     OF        XFDLIB
     JOINING     YTI         AS        PREFIX.
*
******************************************************************
 LINKAGE                SECTION.
 01  PARA-IN-SELKBN        PIC   X(01).
 01  PARA-IN-ONLNO         PIC   9(10).
 01  PARA-IN-TDATE         PIC   9(08).
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION
                           USING PARA-IN-SELKBN
                                 PARA-IN-ONLNO
                                 PARA-IN-TDATE.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNDTKNL1.
     MOVE      "DNDTKNL1"    TO   ERR-FL-ID.
     MOVE      DND1-STATUS   TO   ERR-STCD.
     DISPLAY   MSG-ABEND1        UPON CONS.
     DISPLAY   MSG-ABEND2        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNDTKNL3.
     MOVE      "DNDTKNL3"    TO   ERR-FL-ID.
     MOVE      DND3-STATUS   TO   ERR-STCD.
     DISPLAY   MSG-ABEND1        UPON CONS.
     DISPLAY   MSG-ABEND2        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYYOTEER.
     MOVE   "NYYOTEER"        TO    ERR-FL-ID.
     MOVE    CSV-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
*FILEERR-SEC2           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  DSPF.
*    MOVE   "DSPF    "        TO    ERR-FL-ID.
*    MOVE    DSP-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
***
*FILEERR-SEC3           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  DNDTKNL1.
*    MOVE   "DNDTKNL1"        TO    ERR-FL-ID.
*    MOVE    NYY-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBMEIL7.
     MOVE   "SUBMEIL7"        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
*FILEERR-SEC5           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  ZSOKMS1.
*    MOVE   "ZSOKMS1 "        TO    ERR-FL-ID.
*    MOVE    SOK-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
***
*FILEERR-SEC6           SECTION.
*    USE AFTER     EXCEPTION
*                  PROCEDURE  JYOKEN1.
*    MOVE   "JYOKEN1 "        TO    ERR-FL-ID.
*    MOVE    JYO-STATUS       TO    ERR-STCD.
*    DISPLAY MSG-ABEND1       UPON  CONS.
*    DISPLAY MSG-ABEND2       UPON  CONS.
*    MOVE    4000             TO    PROGRAM-STATUS.
*    STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 NVD0810V-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 NVD0810V-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
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
     OPEN     INPUT     DNDTKNL1  DNDTKNL3  SUBMEIL7.
     OPEN     OUTPUT    NYYOTEER.
*
 010-INIT.
*ＮＡＶＳ受信データ管理ファイルL1 スタート
     IF   PARA-IN-SELKBN = "1"
          PERFORM  DNDTKNL1-START-SEC
          IF   END-FLG = "END"
               DISPLAY NC"対象データなし" UPON CONS
               GO                    TO   INIT-END
          END-IF
     END-IF.
*ＮＡＶＳ受信データ管理ファイルL1 読込
     IF   PARA-IN-SELKBN = "1"
     PERFORM DNDTKNL1-READ-SEC
          IF   END-FLG = "END"
               DISPLAY NC"対象データなし" UPON CONS
               GO                    TO   INIT-END
          END-IF
     END-IF.
*
 011-INIT.
*ＮＡＶＳ受信データ管理ファイルL3 スタート
     IF   PARA-IN-SELKBN = "2"
          PERFORM  DNDTKNL3-START-SEC
          IF   END-FLG = "END"
               DISPLAY NC"対象データなし" UPON CONS
               GO                    TO   INIT-END
          END-IF
     END-IF.
*ＮＡＶＳ受信データ管理ファイルL3 読込
     IF   PARA-IN-SELKBN = "2"
     PERFORM DNDTKNL3-READ-SEC
          IF   END-FLG = "END"
               DISPLAY NC"対象データなし" UPON CONS
               GO                    TO   INIT-END
          END-IF
     END-IF.
*
 020-INIT.
*    項目タイトルレコード出力
     MOVE     SPACE                   TO       CSV1-REC.
     MOVE     X"28"                   TO       CSV1-KS01
                                               CSV1-KS02
                                               CSV1-KS03
                                               CSV1-KS04
                                               CSV1-KS05
                                               CSV1-KS06
                                               CSV1-KS07
                                               CSV1-KS08
                                               CSV1-KS09
                                               CSV1-KS10
                                               CSV1-KS11
                                               CSV1-KS12
                                               CSV1-KS13
                                               CSV1-KS14
                                               CSV1-KS15
                                               CSV1-KS16
                                               CSV1-KS17
                                               CSV1-KS18
                                               CSV1-KS19
                                               CSV1-KS20
                                               CSV1-KS21
                                               CSV1-KS22
                                               CSV1-KS23
                                               CSV1-KS24
                                               CSV1-KS25
                                               CSV1-KS26
                                               CSV1-KS27.
     MOVE     X"29"                   TO       CSV1-KE01
                                               CSV1-KE02
                                               CSV1-KE03
                                               CSV1-KE04
                                               CSV1-KE05
                                               CSV1-KE06
                                               CSV1-KE07
                                               CSV1-KE08
                                               CSV1-KE09
                                               CSV1-KE10
                                               CSV1-KE11
                                               CSV1-KE12
                                               CSV1-KE13
                                               CSV1-KE14
                                               CSV1-KE15
                                               CSV1-KE16
                                               CSV1-KE17
                                               CSV1-KE18
                                               CSV1-KE19
                                               CSV1-KE20
                                               CSV1-KE21
                                               CSV1-KE22
                                               CSV1-KE23
                                               CSV1-KE24
                                               CSV1-KE25
                                               CSV1-KE26
                                               CSV1-KE27.
     MOVE     ","                     TO       CSV1-KK01
                                               CSV1-KK02
                                               CSV1-KK03
                                               CSV1-KK04
                                               CSV1-KK05
                                               CSV1-KK06
                                               CSV1-KK07
                                               CSV1-KK08
                                               CSV1-KK09
                                               CSV1-KK10
                                               CSV1-KK11
                                               CSV1-KK12
                                               CSV1-KK13
                                               CSV1-KK14
                                               CSV1-KK15
                                               CSV1-KK16
                                               CSV1-KK17
                                               CSV1-KK18
                                               CSV1-KK19
                                               CSV1-KK20
                                               CSV1-KK21
                                               CSV1-KK22
                                               CSV1-KK23
                                               CSV1-KK24
                                               CSV1-KK25
                                               CSV1-KK26
                                               CSV1-KK27.
     MOVE   NC"送受信ＮＯ"             TO      CSV1-K01.
     MOVE   NC"取込日付"               TO      CSV1-K02.
     MOVE   NC"取込時刻"               TO      CSV1-K03.
     MOVE   NC"伝票番号"               TO      CSV1-K04.
     MOVE   NC"行番号"                 TO      CSV1-K05.
     MOVE   NC"発注日"                 TO      CSV1-K06.
     MOVE   NC"納品日"                 TO      CSV1-K07.
     MOVE   NC"新商品ＣＤ"             TO      CSV1-K08.
     MOVE   NC"旧商品ＣＤ"             TO      CSV1-K09.
     MOVE   NC"ＪＡＮＣＤ"             TO      CSV1-K10.
     MOVE   NC"商品名"                 TO      CSV1-K11.
     MOVE   NC"倉庫"                   TO      CSV1-K12.
     MOVE   NC"場所"                   TO      CSV1-K13.
     MOVE   NC"入荷数"                 TO      CSV1-K14.
     MOVE   NC"単価"                   TO      CSV1-K15.
     MOVE   NC"金額"                   TO      CSV1-K16.
     MOVE   NC"エラー区分"             TO      CSV1-K17.
     MOVE   NC"ＥＲ１：伝票_重複"     TO      CSV1-K18.
     MOVE   NC"ＥＲ２：商品未登録"     TO      CSV1-K19.
     MOVE   NC"ＥＲ３：場所未登録"     TO      CSV1-K20.
     MOVE   NC"ＥＲ４：仕入先未登録"   TO      CSV1-K21.
     MOVE   NC"ＥＲ５：日付エラー"     TO      CSV1-K22.
     MOVE   NC"ＥＲ６：ＪＡＮＣＤ空白" TO      CSV1-K23.
     MOVE   NC"ＥＲ７："               TO      CSV1-K24.
     MOVE   NC"ＥＲ８："               TO      CSV1-K25.
     MOVE   NC"ＥＲ９："               TO      CSV1-K26.
     MOVE   NC"ＥＲ１０："             TO      CSV1-K27.
     WRITE  CSV-REC                    FROM    CSV1-REC.
*
 INIT-END.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
*
     ADD        1           TO      DNDTKNF-CNT.
*
*入荷予定エラーリストＣＳＶ出力
 MAIN-01.
*
     IF       PARA-IN-SELKBN  = "1"
              PERFORM  NYYOTEER-WRITE1-SEC
     ELSE
              PERFORM  NYYOTEER-WRITE2-SEC
     END-IF.
*
 MAIN-02.
     IF       PARA-IN-SELKBN  =  "1"
              PERFORM  DNDTKNL1-READ-SEC
     ELSE
              PERFORM  DNDTKNL3-READ-SEC
     END-IF.
*    IF       PARA-IN-SELKBN  = "2"
*             GO              TO  MAIN-02
*    END-IF.
*    READ     DNDTKNL1
*       AT END   MOVE  "END"  TO  END-FLG
*                GO TO MAIN-END
*    END-READ.
*    GO TO MAIN-END.
*
*MAIN-02.
*    READ     DNDTKNL3
*       AT END   MOVE  "END"  TO  END-FLG
*                GO TO MAIN-END
*    END-READ.
*
 MAIN-END.
     EXIT.
****************************************************************
*    ＮＡＶＳ受信データ管理ファイルL1 スタート
****************************************************************
 DNDTKNL1-START-SEC          SECTION.
*
*    MOVE    "DNDTKNL1-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   DND1-REC.
     INITIALIZE                        DND1-REC.
     MOVE     PARA-IN-ONLNO       TO   DND1-F01.
*
     START  DNDTKNL1  KEY  IS  >=      DND1-F01
                                       DND1-F07
                                       DND1-F08
                                       DND1-F09
                                       DND1-F10
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 DNDTKNL1-START-EXIT.
     EXIT.
*
****************************************************************
*    ＮＡＶＳ受信データ管理ファイルL1 読込
****************************************************************
 DNDTKNL1-READ-SEC           SECTION.
*
*    MOVE    "DNDTKNL1-READ-SEC"  TO   S-NAME.
*
 DNDTKNL1-READ-010.
     READ     DNDTKNL1       NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNDTKNL1-READ-EXIT
     END-READ.
*送受信指示NOを判定
     IF       DND1-F01   NOT =  PARA-IN-ONLNO
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNDTKNL1-READ-EXIT
     END-IF.
*エラー区分チェック
     IF       DND1-F22   NOT =  "1"
              GO                  TO   DNDTKNL1-READ-010
     END-IF.
*件数カウント
     ADD      1                   TO   DNDTKNF-CNT.
     MOVE     DND1-F20            TO   YTI-REC.
*
 DNDTKNL1-READ-EXIT.
     EXIT.
*
****************************************************************
*    ＮＡＶＳ受信データ管理ファイルL3 スタート
****************************************************************
 DNDTKNL3-START-SEC          SECTION.
*
*    MOVE    "DNDTKNL3-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   DND3-REC.
     INITIALIZE                        DND3-REC.
     MOVE     PARA-IN-TDATE       TO   DND3-F07.
*
     START  DNDTKNL3  KEY  IS  >=      DND3-F07
                                       DND3-F08
                                       DND3-F09
                                       DND3-F10
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 DNDTKNL3-START-EXIT.
     EXIT.
*
****************************************************************
*    ＮＡＶＳ受信データ管理ファイルL3 読込
****************************************************************
 DNDTKNL3-READ-SEC           SECTION.
*
*    MOVE    "DNDTKNL3-READ-SEC"   TO   S-NAME.
*
 DNDTKNL3-READ-010.
     READ     DNDTKNL3       NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNDTKNL3-READ-EXIT
     END-READ.
*取込日付を判定
     IF       DND3-F07   NOT =  PARA-IN-TDATE
              MOVE     "END"      TO   END-FLG
              GO                  TO   DNDTKNL3-READ-EXIT
     END-IF.
*エラー区分チェック
     IF       DND3-F22   NOT =  "1"
              GO                  TO   DNDTKNL3-READ-010
     END-IF.
*件数カウント
     ADD      1                   TO   DNDTKNF-CNT.
     MOVE     DND3-F20            TO   YTI-REC.
*
 DNDTKNL3-READ-EXIT.
     EXIT.
*
************************************************************
*      １   入荷予定リストＣＳＶ出力　L1より　　　         *
************************************************************
 NYYOTEER-WRITE1-SEC       SECTION.
*
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-MK01 CSV-MK02 CSV-MK03
                                    CSV-MK04 CSV-MK05 CSV-MK06
                                    CSV-MK07 CSV-MK08 CSV-MK09
                                    CSV-MK10 CSV-MK11 CSV-MK12
                                    CSV-MK13 CSV-MK14 CSV-MK15
                                    CSV-MK16 CSV-MK17 CSV-MK18
                                    CSV-MK19 CSV-MK20 CSV-MK21
                                    CSV-MK22 CSV-MK23 CSV-MK24
                                    CSV-MK25 CSV-MK26 CSV-MK27.
     MOVE     X"28"         TO      CSV-MS11.
     MOVE     X"29"         TO      CSV-ME11.
*カウントアップ
     ADD       1            TO      NYYOTEER-CNT.
*
*送受信指示_
     MOVE      DND1-F01      TO      CSV-M01.
*取込日付
     MOVE      DND1-F07(1:4) TO      CSV-M02(1:4).
     MOVE      "/"           TO      CSV-M02(5:1).
     MOVE      DND1-F07(5:2) TO      CSV-M02(6:2).
     MOVE      "/"           TO      CSV-M02(8:1).
     MOVE      DND1-F07(7:2) TO      CSV-M02(9:2).
*取込時刻
     MOVE      DND1-F08(1:2) TO      CSV-M03(1:2).
     MOVE      ":"           TO      CSV-M03(3:1).
     MOVE      DND1-F08(3:2) TO      CSV-M03(4:2).
     MOVE      ":"           TO      CSV-M03(6:1).
     MOVE      DND1-F08(5:2) TO      CSV-M03(7:2).
*伝票番号
     MOVE      DND1-F09      TO      CSV-M04.
*行_
     MOVE      DND1-F10      TO      CSV-M05.
*発注日
     MOVE      YTI-F04       TO      CSV-M06.
*納品日
     MOVE      YTI-F05       TO      CSV-M07.
*新商品コード
     MOVE      YTI-F06       TO      CSV-M08.
*旧商品コード
     MOVE      YTI-F07       TO      CSV-M09.
*ＪＡＮＣＤ
     MOVE      YTI-F08       TO      CSV-M10.
*商品名
*　商品名称マスタ検索
     MOVE      SPACE         TO      MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE      YTI-F08       TO      MEI-D01.
*TEST
*    DISPLAY "YTI-F08 = " UPON CONS
*TEST
     READ      SUBMEIL7
       INVALID
         MOVE  ALL NC"＊"    TO      CSV-M11
       NOT INVALID
         MOVE  MEI-F02       TO      CSV-M11
     END-READ.
*入庫倉庫コード
     MOVE      YTI-F15       TO      CSV-M12.
*入庫場所コード
     MOVE      YTI-F16       TO      CSV-M13.
*入荷数符号
     MOVE      YTI-F10       TO      CSV-MA14.
*入荷数
*    MOVE      YTI-F11       TO      CSV-M14.
     MOVE      YTI-F11       TO      HENKAN1.
     MOVE      HENKAN1-1     TO      HENKAN1-DATA1.
     MOVE      "."           TO      HENKAN1-DATA2.
     MOVE      HENKAN1-2     TO      HENKAN1-DATA3.
     MOVE      HENKAN1-DATA  TO      CSV-M14.
*単価
*    MOVE      YTI-F12       TO      CSV-M15.
     MOVE      YTI-F12       TO      HENKAN1.
     MOVE      HENKAN1-1     TO      TANKA1-DATA1.
     MOVE      "."           TO      TANKA1-DATA2.
     MOVE      HENKAN1-2     TO      TANKA1-DATA3.
     MOVE      TANKA1-DATA   TO      CSV-M15.
*入荷金額符号
     MOVE      YTI-F13       TO      CSV-MA16.
*入荷金額
     MOVE      YTI-F14       TO      CSV-M16.
*エラー区分
     MOVE      DND1-F22      TO      CSV-M17.
*エラー１
     MOVE      DND1-F23      TO      CSV-M18.
*エラー2
     MOVE      DND1-F24      TO      CSV-M19.
*エラー3
     MOVE      DND1-F25      TO      CSV-M20.
*エラー4
     MOVE      DND1-F26      TO      CSV-M21.
*エラー5
     MOVE      DND1-F27      TO      CSV-M22.
*エラー6
     MOVE      DND1-F28      TO      CSV-M23.
*エラー7
     MOVE      DND1-F29      TO      CSV-M24.
*エラー8
     MOVE      DND1-F30      TO      CSV-M25.
*エラー9
     MOVE      DND1-F31      TO      CSV-M26.
*エラー10
     MOVE      DND1-F32      TO      CSV-M27.
*
*レコード出力
     WRITE     CSV-REC.

 NYYOTEER-WRITE1-EXIT.
     EXIT.
************************************************************
*      １   入荷予定リストＣＳＶ出力　L3より　　　         *
************************************************************
 NYYOTEER-WRITE2-SEC       SECTION.
*
*レコード初期化　　　　　　　　　　
     MOVE     SPACE         TO      CSV-REC.
     INITIALIZE                     CSV-REC.
*カンマセット
     MOVE     ","           TO      CSV-MK01 CSV-MK02 CSV-MK03
                                    CSV-MK04 CSV-MK05 CSV-MK06
                                    CSV-MK07 CSV-MK08 CSV-MK09
                                    CSV-MK10 CSV-MK11 CSV-MK12
                                    CSV-MK13 CSV-MK14 CSV-MK15
                                    CSV-MK16 CSV-MK17 CSV-MK18
                                    CSV-MK19 CSV-MK20 CSV-MK21
                                    CSV-MK22 CSV-MK23 CSV-MK24
                                    CSV-MK25 CSV-MK26 CSV-MK27.
     MOVE     X"28"         TO      CSV-MS11.
     MOVE     X"29"         TO      CSV-ME11.
*カウントアップ
     ADD       1            TO      NYYOTEER-CNT.
*
*送受信指示_
     MOVE      DND3-F01      TO      CSV-M01.
*取込日付
     MOVE      DND3-F07(1:4) TO      CSV-M02(1:4).
     MOVE      "/"           TO      CSV-M02(5:1).
     MOVE      DND3-F07(5:2) TO      CSV-M02(6:2).
     MOVE      "/"           TO      CSV-M02(8:1).
     MOVE      DND3-F07(7:2) TO      CSV-M02(9:2).
*取込時刻
     MOVE      DND3-F08(1:2) TO      CSV-M03(1:2).
     MOVE      ":"           TO      CSV-M03(3:1).
     MOVE      DND3-F08(3:2) TO      CSV-M03(4:2).
     MOVE      ":"           TO      CSV-M03(6:1).
     MOVE      DND3-F08(5:2) TO      CSV-M03(7:2).
*伝票番号
     MOVE      DND3-F09      TO      CSV-M04.
*行_
     MOVE      DND3-F10      TO      CSV-M05.
*発注日
     MOVE      YTI-F04       TO      CSV-M06.
*納品日
     MOVE      YTI-F05       TO      CSV-M07.
*新商品コード
     MOVE      YTI-F06       TO      CSV-M08.
*旧商品コード
     MOVE      YTI-F07       TO      CSV-M09.
*ＪＡＮＣＤ
     MOVE      YTI-F08       TO      CSV-M10.
*商品名
*　商品名称マスタ検索
     MOVE      SPACE         TO      MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE      YTI-F08       TO      MEI-F06.
     READ      SUBMEIL7
       INVALID
         MOVE  ALL NC"＊"    TO      CSV-M11
       NOT INVALID
         MOVE  MEI-F02       TO      CSV-M11
     END-READ.
*入庫倉庫コード
     MOVE      YTI-F15       TO      CSV-M12.
*入庫場所コード
     MOVE      YTI-F16       TO      CSV-M13.
*入荷数符号
     MOVE      YTI-F10       TO      CSV-MA14.
*入荷数
     MOVE      YTI-F11       TO      HENKAN1.
     MOVE      HENKAN1-1    TO       HENKAN1-DATA1.
     MOVE      "."          TO       HENKAN1-DATA2.
     MOVE      HENKAN1-2    TO       HENKAN1-DATA3.
     MOVE      HENKAN1-DATA TO       CSV-M14.
*単価
     MOVE      YTI-F12      TO       HENKAN1.
     MOVE      HENKAN1-1    TO       HENKAN1-DATA1.
     MOVE      "."          TO       HENKAN1-DATA2.
     MOVE      HENKAN1-2    TO       HENKAN1-DATA3.
     MOVE      HENKAN1-DATA TO       CSV-M15.
*入荷金額符号
     MOVE      YTI-F13       TO      CSV-MA16.
*入荷金額
     MOVE      YTI-F14       TO      CSV-M16.
*エラー区分
     MOVE      DND3-F22      TO      CSV-M17.
*エラー１
     MOVE      DND3-F23      TO      CSV-M18.
*エラー2
     MOVE      DND3-F24      TO      CSV-M19.
*エラー3
     MOVE      DND3-F25      TO      CSV-M20.
*エラー4
     MOVE      DND3-F26      TO      CSV-M21.
*エラー5
     MOVE      DND3-F27      TO      CSV-M22.
*エラー6
     MOVE      DND3-F28      TO      CSV-M23.
*エラー7
     MOVE      DND3-F29      TO      CSV-M24.
*エラー8
     MOVE      DND3-F30      TO      CSV-M25.
*エラー9
     MOVE      DND3-F31      TO      CSV-M26.
*エラー10
     MOVE      DND3-F32      TO      CSV-M27.
*
*レコード出力
     WRITE     CSV-REC.

 NYYOTEER-WRITE2-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE   NYYOTEER DNDTKNL1 DNDTKNL3 SUBMEIL7.
*
     IF      DNDTKNF-CNT NOT = ZERO
             DISPLAY  " DNDTKNF-READ-CNT   = " DNDTKNF-CNT
                                                    UPON CONS
             DISPLAY  " NYYOTEER-WRITE-CNT = " NYYOTEER-CNT
                                                    UPON CONS
     ELSE
             DISPLAY  " DNDTKNF-READ-CNT  = "  DNDTKNF-CNT
                                                    UPON CONS
             DISPLAY  " NYYOTEER-WRITE-CNT = " NYYOTEER-CNT
                                                    UPON CONS
             MOVE    4011      TO     PROGRAM-STATUS
     END-IF.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
