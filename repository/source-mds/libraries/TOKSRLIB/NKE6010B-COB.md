# NKE6010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE6010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷検品　　　　　　　            *
*    モジュール名　　　　：　出荷エラー梱包データ抽出　　　　　*
*    処理概要　　　　　　：　検品結果梱包ファイル（カインズ）　*
*                            から梱包データを抽出する。        *
*                            　　　　　　　　　　　　　　　　　*
*    流用　　　　　　　　：　NKE6010B                          *
*    作成日／作成者　　　：　2024/04/22 NAV                    *
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE6010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2024/04/22.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*検品結果抽出ファイル（カインズ）
     SELECT    CZSUMWW1 ASSIGN    TO        DA-01-VI-CZSUMWW1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SMW-F01
                                            SMW-F02
                                            SMW-F03
                                            SMW-F04
                                            SMW-F05
                                            SMW-F06
                                            SMW-F07
                                            SMW-F08
                                            SMW-F10
                                            SMW-F11
                                            SMW-F09
                        FILE      STATUS    SMW-STATUS.
*検品結果梱包ファイル（カインズ）
     SELECT    CZKONTK3 ASSIGN    TO        DA-01-VI-CZKONTK3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       KON-F01
                                            KON-F02
                                            KON-F03
                                            KON-F04
                                            KON-F05
                                            KON-F06
                                            KON-F07
                                            KON-F08
                                            KON-F10
                                            KON-F11
                                            KON-F09
                        FILE      STATUS    KON-STATUS.
*出荷エラー梱包データ（カインズ）
     SELECT   CZKONWWF  ASSIGN    TO        DA-01-VS-CZKONWWF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   KNW-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*検品結果抽出ファイル（カインズ）
 FD  CZSUMWW1           LABEL RECORD   IS   STANDARD.
     COPY     CZSUMWW1  OF        XFDLIB
     JOINING  SMW       AS        PREFIX.
*検品結果梱包ファイル（カインズ）
 FD  CZKONTK3           LABEL RECORD   IS   STANDARD.
     COPY     CZKONTK3  OF        XFDLIB
     JOINING  KON       AS        PREFIX.
*出荷エラー梱包データ（カインズ）
 FD  CZKONWWF           LABEL RECORD   IS   STANDARD.
     COPY     CZKONWWF  OF        XFDLIB
     JOINING  KNW       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  CZSUMWW1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  CZKONWWF-WRT-CNT        PIC  9(07)     VALUE  ZERO.
 01  CZKONTK3-INV-FLG        PIC  X(03)     VALUE  SPACE.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE            PIC 9(06).
     03  SYS-DATEW           PIC 9(08).
*ステータス
 01  WK-ST.
     03  SMW-STATUS          PIC  X(02).
     03  KON-STATUS          PIC  X(02).
     03  KNW-STATUS          PIC  X(02).
*BRK項目　エリア
*    03  BRK-SUM-F95         PIC  9(06)    VALUE ZERO.
*    03  BRK-SUM-F95-FLG     PIC  X(03)    VALUE SPACE.
*    03  BRK-SUM-F03         PIC  9(08)    VALUE ZERO.
*    03  BRK-SUM-F03-FLG     PIC  X(03)    VALUE SPACE.
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  ST-PG           PIC   X(08)  VALUE "NKE6010B".
         05  FILLER          PIC   X(11)  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "NKE6010B".
         05  FILLER          PIC   X(11)  VALUE " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "NKE6010B".
         05  FILLER          PIC   X(11)  VALUE " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  AB-FILE         PIC   X(08).
         05  FILLER          PIC   X(06)  VALUE " ST = ".
         05  AB-STS          PIC   X(02).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(07)  VALUE " SEC = ".
         05  S-NAME          PIC   X(30).
*
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-IN-BUMON         PIC   X(04).
*01  PARA-IN-TANCD         PIC   X(02).
*01  PARA-IN-SOKCD         PIC   X(02).
*01  PARA-IN-CKUBUN        PIC   X(01).
*01  PARA-IN-TDATE         PIC   9(08).
*01  PARA-IN-TTIME         PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*                                USING PARA-IN-BUMON
*                                      PARA-IN-TANCD
*                                      PARA-IN-SOKCD
*                                      PARA-IN-CKUBUN
*                                      PARA-IN-TDATE
*                                      PARA-IN-TTIME.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CZSUMWW1.
     MOVE      "CZSUMWW1"   TO      AB-FILE.
     MOVE      SMW-STATUS   TO      AB-STS.
     DISPLAY   MSG-ABEND            UPON CONS.
     DISPLAY   SEC-NAME             UPON CONS.
     DISPLAY   ABEND-FILE           UPON CONS.
     MOVE      4000         TO      PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CZKONTK3.
     MOVE      "CZKONTK3"   TO      AB-FILE.
     MOVE      KON-STATUS   TO      AB-STS.
     DISPLAY   MSG-ABEND            UPON CONS.
     DISPLAY   SEC-NAME             UPON CONS.
     DISPLAY   ABEND-FILE           UPON CONS.
     MOVE      4000         TO      PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CZKONWWF.
     MOVE      "CZKONWWF"   TO      AB-FILE.
     MOVE      KNW-STATUS   TO      AB-STS.
     DISPLAY   MSG-ABEND            UPON CONS.
     DISPLAY   SEC-NAME             UPON CONS.
     DISPLAY   ABEND-FILE           UPON CONS.
     MOVE      4000         TO      PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE    "PROCESS-START"    TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE    "INIT-SEC"         TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     CZSUMWW1 CZKONTK3.
     OPEN     OUTPUT    CZKONWWF.
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
                 MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
                 MOVE ZERO           TO   SYS-DATEW
     END-IF.
     ACCEPT      SYS-TIME          FROM   TIME.
*検品結果集計ファイル読込
     PERFORM     CZSUMWW1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし。＃＃"  UPON CONS
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    検品結果抽出ファイル読込
****************************************************************
 CZSUMWW1-READ-SEC           SECTION.
*
     MOVE   "CZSUMWW1-READ-SEC"   TO   S-NAME.
*
 CZSUMTK1-READ-01.
     READ     CZSUMWW1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   CZSUMWW1-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   CZSUMWW1-READ-CNT.
*
 CZSUMWW1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*検品結果梱包ファイルSTART-初期READ
     MOVE     "   "      TO   CZKONTK3-INV-FLG.
     PERFORM  CZKONTK3-START-SEC.
     IF       CZKONTK3-INV-FLG = "INV"
              GO         TO   MAIN-999
     END-IF.
 MAIN-020.
*対象検品結果梱包ファイル存在時
     INITIALIZE               KNW-REC.
     MOVE     KON-REC    TO   KNW-REC.
     WRITE                    KNW-REC.
     ADD      1          TO   CZKONWWF-WRT-CNT.
 MAIN-030.
*検品結果梱包ファイル次READ
     MOVE     "   "      TO   CZKONTK3-INV-FLG.
     PERFORM  CZKONTK3-READ-SEC.
     IF       CZKONTK3-INV-FLG = "INV"
              GO      TO     MAIN-999
     ELSE
              GO      TO     MAIN-020
     END-IF.
*
 MAIN-999.
     PERFORM  CZSUMWW1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*    検品結果梱包ファイルスタート
****************************************************************
 CZKONTK3-START-SEC          SECTION.
*
     MOVE   "CZKONTK3-START-SEC"  TO   S-NAME.
*
 CZKONTK3-START-01.
     MOVE     SPACE               TO   KON-REC.
     INITIALIZE                        KON-REC.
*
     MOVE     SMW-F01             TO   KON-F01.
     MOVE     SMW-F02             TO   KON-F02.
     MOVE     SMW-F03             TO   KON-F03.
     MOVE     SMW-F04             TO   KON-F04.
     MOVE     SMW-F05             TO   KON-F05.
     MOVE     SMW-F06             TO   KON-F06.
     MOVE     SMW-F07             TO   KON-F07.
     MOVE     SMW-F08             TO   KON-F08.
     MOVE     SMW-F10             TO   KON-F10.
     MOVE     SMW-F11             TO   KON-F11.
*
     START    CZKONTK3  KEY  IS  >=    KON-F01 KON-F02
                                       KON-F03 KON-F04
                                       KON-F05 KON-F06
                                       KON-F07 KON-F08
                                       KON-F10 KON-F11
              INVALID
                 GO                    TO   CZKONTK3-START-03
              NOT INVALID
                 GO                    TO   CZKONTK3-START-02
     END-START.
*
 CZKONTK3-START-02.
*検品結果梱包ファイル初期ＲＥＡＤ
     PERFORM  CZKONTK3-READ-SEC.
     IF       CZKONTK3-INV-FLG = "INV"
              GO                    TO   CZKONTK3-START-03
     ELSE
              GO                    TO   CZKONTK3-START-EXIT
     END-IF.
*
 CZKONTK3-START-03.
     MOVE    "INV"         TO   CZKONTK3-INV-FLG.
     DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS.
     DISPLAY NC"梱包ファイルＫＥＹなし！？"   UPON CONS.
     DISPLAY NC"　・バッチ日付　　＝" SMW-F01 UPON CONS.
     DISPLAY NC"　・バッチ時刻　　＝" SMW-F02 UPON CONS.
     DISPLAY NC"　・取引先ＣＤ　　＝" SMW-F03 UPON CONS.
     DISPLAY NC"　・倉庫ＣＤ　　　＝" SMW-F04 UPON CONS.
     DISPLAY NC"　・センター納品日＝" SMW-F05 UPON CONS.
     DISPLAY NC"　・センターＣＤ　＝" SMW-F06 UPON CONS.
     DISPLAY NC"　・店舗納品日　　＝" SMW-F07 UPON CONS.
     DISPLAY NC"　・店舗ＣＤ　　　＝" SMW-F08 UPON CONS.
     DISPLAY NC"　・伝票番号　　　＝" SMW-F10 UPON CONS.
     DISPLAY NC"　・行番号　　　　＝" SMW-F11 UPON CONS.
     DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS.
     GO                    TO   CZKONTK3-START-EXIT.
*
 CZKONTK3-START-EXIT.
     EXIT.
*
****************************************************************
*    検品結果梱包ファイル読込
****************************************************************
 CZKONTK3-READ-SEC           SECTION.
*
     MOVE    "CZKONTK3-READ-SEC"   TO   S-NAME.
*
 CZKONTK3-READ-01.
     READ     CZKONTK3  NEXT  AT  END
              MOVE     "INV"      TO   CZKONTK3-INV-FLG
              GO                  TO   CZKONTK3-READ-EXIT
     END-READ.
*対象判定
     IF     ( KON-F01  =  SMW-F01 ) AND
            ( KON-F02  =  SMW-F02 ) AND
            ( KON-F03  =  SMW-F03 ) AND
            ( KON-F04  =  SMW-F04 ) AND
            ( KON-F05  =  SMW-F05 ) AND
            ( KON-F06  =  SMW-F06 ) AND
            ( KON-F07  =  SMW-F07 ) AND
            ( KON-F08  =  SMW-F08 ) AND
            ( KON-F10  =  SMW-F10 ) AND
            ( KON-F11  =  SMW-F11 )
              CONTINUE
     ELSE
              MOVE    "INV"       TO   CZKONTK3-INV-FLG
              GO                  TO   CZKONTK3-READ-EXIT
     END-IF.
*
 CZKONTK3-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
*  ＝検品結果抽出ファイル読込件数
     DISPLAY "CZSUMWW1 READ-CNT  = " CZSUMWW1-READ-CNT UPON CONS.
*  ＝出荷エラー梱包データ出力件数
     DISPLAY "CZKONWWF WRT-CNT   = " CZKONWWF-WRT-CNT  UPON CONS.
*
     CLOSE     CZSUMWW1
               CZKONTK3
               CZKONWWF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
