****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　カインズ流通ＢＭＳ　　　　　　　　*
*    業務名　　　　　　　：　返品業務　　　　　　　　　　　　　*
*    モジュール名　　　　：　返品伝票データ抽出VER3            *
*    作成日／更新日　　　：　2014/05/21                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　流通ＢＭＳ返品伝票メッセージより　*
*                            返品伝票ワークを出力する。　　　　*
*                            VER3:「税項目」の抽出あり　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBMC050B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/05/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*返品伝票メッセージ(受信日)
     SELECT   BMSHEPF1  ASSIGN    TO        DA-01-VI-BMSHEPL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEP1-F013 HEP1-F011
                                            HEP1-F012
                        FILE  STATUS   IS   HEP1-STS.
*返品伝票メッセージ(計上日)
     SELECT   BMSHEPF2  ASSIGN    TO        DA-01-VI-BMSHEPL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEP2-F013 HEP2-F330
                        FILE  STATUS   IS   HEP2-STS.
*返品伝票ワークVER3
     SELECT   BMSHE3W   ASSIGN    TO        DA-01-VS-BMSHE3W
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   HEW-STS.
*取引先マスタ
     SELECT  HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    返品伝票メッセージ(受信日)　　　　
******************************************************************
 FD  BMSHEPF1           LABEL RECORD   IS   STANDARD.
     COPY     BMSHEPF   OF        XFDLIB
              JOINING   HEP1      PREFIX.
******************************************************************
*    返品伝票メッセージ(計上日)　　　　
******************************************************************
 FD  BMSHEPF2           LABEL RECORD   IS   STANDARD.
     COPY     BMSHEPF   OF        XFDLIB
              JOINING   HEP2      PREFIX.
******************************************************************
*    返品伝票ワークVER3
******************************************************************
 FD  BMSHE3W            LABEL RECORD   IS   STANDARD.
     COPY     BMSHE3W   OF        XFDLIB
              JOINING   HEW       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  READ-CNT                PIC  9(07)     VALUE  ZERO.
 01  WRITE-CNT               PIC  9(07)     VALUE  ZERO.
 01  UNMACH-CNT              PIC  9(07)     VALUE  ZERO.
*
*
******************************************************************
*    返品伝票メッセージ　ワーク定義　
******************************************************************
** WHEP-REC.
     COPY     BMSHEPF   OF        XFDLIB
              JOINING   WHEP      PREFIX.
*システム日付の編集
 01  SYS-WORKAREA.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*
 01  STS-AREA.
     03  HEP1-STS          PIC  X(02).
     03  HEP2-STS          PIC  X(02).
     03  HEW-STS           PIC  X(02).
*
     03  TOK-STS           PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBMC050B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBMC050B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBMC050B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TOKCD     PIC   9(08).
 01  PARA-HSYU      PIC   X(01).
 01  PARA-HFROM     PIC   9(08).
 01  PARA-HTO       PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING  PARA-TOKCD  PARA-HSYU
                                        PARA-HFROM  PARA-HTO.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSHEPF1.
     MOVE      "BMSHEPL2"   TO   AB-FILE.
     MOVE      HEP1-STS     TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSHEPF2.
     MOVE      "BMSHEPL3"   TO   AB-FILE.
     MOVE      HEP2-STS     TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSHE3W.
     MOVE      "BMSHE3W"    TO   AB-FILE.
     MOVE      HEW-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS2"    TO   AB-FILE.
     MOVE      TOK-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     IF  PARA-HSYU  =  "1"
         OPEN  INPUT    BMSHEPF1
     END-IF.
     IF  PARA-HSYU  =  "2"
         OPEN  INPUT    BMSHEPF2
     END-IF.
*
     MOVE     SPACE               TO   WHEP-REC.
     INITIALIZE                        WHEP-REC.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    BMSHE3W.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*ファイルスタート
     PERFORM  INSTART-SEC.
     IF   END-FLG = "END"
      DISPLAY NC"＃＃　返品伝票メッセージ無　＃＃"  UPON CONS
          GO                    TO   INIT-EXIT
     END-IF.
*ファイル読込
     PERFORM INREAD-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    返品伝票メッセージスタート
****************************************************************
 INSTART-SEC                SECTION.
*
     MOVE    "INSTART-SEC"        TO   S-NAME.
*
     IF  PARA-HSYU  =  "2"
         GO  TO   INSTART-200
     END-IF.
*
     MOVE     PARA-TOKCD          TO   HEP1-F013.
     MOVE     PARA-HFROM          TO   HEP1-F011.
     MOVE     ZERO                TO   HEP1-F012.
* 受信日順
     START  BMSHEPF1 KEY  IS  >=  HEP1-F013   HEP1-F011
                                  HEP1-F012
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
     GO  TO    INSTART-EXIT.
*
 INSTART-200.
     MOVE     PARA-TOKCD          TO   HEP2-F013.
     MOVE     PARA-HFROM          TO   HEP2-F330.
* 計上日順
     START  BMSHEPF2 KEY  IS  >=  HEP2-F013   HEP2-F330
*
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
     GO  TO    INSTART-EXIT.
 INSTART-EXIT.
     EXIT.
*
****************************************************************
*   返品伝票メッセージ読込み
****************************************************************
 INREAD-SEC                 SECTION.
*
     MOVE    "INREAD-SEC"   TO   S-NAME.
*
     IF  PARA-HSYU  =  "2"
         GO  TO   INREAD-200
     END-IF.
     READ     BMSHEPF1   NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   INREAD-EXIT
     END-READ.
     ADD     1    TO   READ-CNT.
     MOVE    HEP1-REC   TO   WHEP-REC.
     GO  TO  INREAD-EXIT.
*
 INREAD-200.
     READ     BMSHEPF2   NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   INREAD-EXIT
     END-READ.
     ADD     1    TO   READ-CNT.
     MOVE    HEP2-REC   TO   WHEP-REC.
*
 INREAD-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
     IF   PARA-HSYU  =  "2"
          GO  TO   MAIN-200
     END-IF.
* 受信日
**  抽出条件判定
     IF   WHEP-F013 >  PARA-TOKCD
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*対象期間終了
     IF  WHEP-F011  >  PARA-HTO
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*返品伝票ワーク出力
*
     PERFORM  HEW-WRITE-SEC.
     GO  TO    MAIN-900.
 MAIN-200.
* 計上日
**  抽出条件判定
     IF   WHEP-F013 >  PARA-TOKCD
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*対象期間終了
     IF  WHEP-F330  >  PARA-HTO
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*
*返品伝票ワーク出力
*
     PERFORM  HEW-WRITE-SEC.
*
     GO  TO    MAIN-900.
 MAIN-900.
     PERFORM  INREAD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数出力
*返品伝票読込
     DISPLAY "BMSHEPF   READ CNT = " READ-CNT UPON CONS.
*返品伝票ワ－ク出力
     DISPLAY "BMSHE3W  WRITE CNT = " WRITE-CNT UPON CONS.
     IF  PARA-HSYU  =  "1"
         CLOSE  BMSHEPF1
     END-IF.
     IF  PARA-HSYU  =  "2"
         CLOSE  BMSHEPF2
     END-IF.
*
     CLOSE     BMSHE3W   HTOKMS.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　レコード出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 HEW-WRITE-SEC         SECTION.
*
     MOVE    "HEW-WRITE-SEC"     TO   S-NAME.
*
     MOVE     SPACE               TO   HEW-REC.
     INITIALIZE                        HEW-REC.
*受信日
     MOVE     WHEP-F011      TO  HEW-F011.
*受信時刻
     MOVE     WHEP-F012      TO  HEW-F012.
*取引先コード
     MOVE     WHEP-F013      TO  HEW-F013.
*
*取引先名
     PERFORM  TOK-READ.
     MOVE     TOK-F02        TO  HEW-F02.
*取引番号
     MOVE     WHEP-F302      TO  HEW-F03.
*最終納品先コード
     MOVE     WHEP-F309      TO  HEW-F04.
*最終納品先名称
     MOVE     WHEP-F311      TO  HEW-F05.
*最終納品先名称カナ
     MOVE     WHEP-F312      TO  HEW-F06.
*計上日
     MOVE     WHEP-F330      TO  HEW-F07.
*処理種別
     MOVE     WHEP-F334      TO  HEW-F08.
*取引明細番号
     MOVE     WHEP-F402      TO  HEW-F09.
*返品・値引理由コード
     MOVE     WHEP-F409      TO  HEW-F10.
*商品コード（GTIN）  ←VER3
     MOVE     WHEP-F411      TO  HEW-F11.
*商品名カナ
     MOVE     WHEP-F416      TO  HEW-F12.
*規格カナ
     MOVE     WHEP-F419      TO  HEW-F13.
*原価金額
     MOVE     WHEP-F445      TO  HEW-F14.
*原単価
     MOVE     WHEP-F446      TO  HEW-F15.
*売価金額
     MOVE     WHEP-F447      TO  HEW-F16.
*売単価
     MOVE     WHEP-F448      TO  HEW-F17.
*返品数量（バラ）
     MOVE     WHEP-F450      TO  HEW-F18.
*自由使用欄半角カナ  ←VER2
     MOVE     WHEP-F344      TO  HEW-F19.
*税区分              ←VER3
     MOVE     WHEP-F340      TO  HEW-F20.
*予備
     MOVE     WHEP-FIL5      TO  HEW-F99.
*
     WRITE   HEW-REC.
     ADD     1      TO      WRITE-CNT.
*
 HPW-WRITE-EXIT.
     EXIT.
*************************************************
 TOK-READ         SECTION.
** 取引先マスタ
     MOVE    WHEP-F013      TO   TOK-F01.
     READ  HTOKMS
       INVALID
         MOVE  ALL  SPACE        TO  TOK-F02
     END-READ.
*
 TOK-READ-EXIT.
     EXIT.
