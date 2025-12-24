****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷検品　　　　　　　            *
*    モジュール名　　　　：　出荷エラー梱包データ更新　　　　　*
*    処理概要　　　　　　：　検品結果梱包ファイル（カインズ）　*
*                            を削除・更新する。　　　　        *
*                            　　　　　　　　　　　　　　　　　*
*    流用　　　　　　　　：　NKE6020B                          *
*    作成日／作成者　　　：　2024/04/23 NAV                    *
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE6020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2024/04/23.
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
*出荷エラー梱包データ（カインズ）
     SELECT    CZKONWW1 ASSIGN    TO        DA-01-VI-CZKONWW1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KNW-F01
                                            KNW-F02
                                            KNW-F03
                                            KNW-F04
                                            KNW-F05
                                            KNW-F06
                                            KNW-F07
                                            KNW-F08
                                            KNW-F10
                                            KNW-F11
                                            KNW-F09
                        FILE      STATUS    KNW-STATUS.
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
*
 DATA                   DIVISION.
 FILE                   SECTION.
*出荷エラー梱包データ（カインズ）
 FD  CZKONWW1           LABEL RECORD   IS   STANDARD.
     COPY     CZKONWW1  OF        XFDLIB
     JOINING  KNW       AS        PREFIX.
*検品結果梱包ファイル（カインズ）
 FD  CZKONTK3           LABEL RECORD   IS   STANDARD.
     COPY     CZKONTK3  OF        XFDLIB
     JOINING  KON       AS        PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  CZKONWW1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  CZKONTK3-RWT-CNT        PIC  9(07)     VALUE  ZERO.
 01  CZKONTK3-DLT-CNT        PIC  9(07)     VALUE  ZERO.
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
     03  KNW-STATUS          PIC  X(02).
     03  KON-STATUS          PIC  X(02).
*BRK項目　エリア
*    03  BRK-SUM-F95         PIC  9(06)    VALUE ZERO.
*    03  BRK-SUM-F95-FLG     PIC  X(03)    VALUE SPACE.
*    03  BRK-SUM-F03         PIC  9(08)    VALUE ZERO.
*    03  BRK-SUM-F03-FLG     PIC  X(03)    VALUE SPACE.
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  ST-PG           PIC   X(08)  VALUE "NKE6020B".
         05  FILLER          PIC   X(11)  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "NKE6020B".
         05  FILLER          PIC   X(11)  VALUE " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "NKE6020B".
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
                        PROCEDURE   CZKONWW1.
     MOVE      "CZKONWW1"   TO      AB-FILE.
     MOVE      KNW-STATUS   TO      AB-STS.
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
     OPEN     INPUT     CZKONWW1.
     OPEN     I-O       CZKONTK3.
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
*出荷エラー梱包データ読込
     PERFORM     CZKONWW1-READ-SEC.
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
*    出荷エラー梱包データ読込
****************************************************************
 CZKONWW1-READ-SEC           SECTION.
*
     MOVE   "CZKONWW1-READ-SEC"   TO   S-NAME.
*
 CZSUMTK1-READ-01.
     READ     CZKONWW1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   CZKONWW1-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   CZKONWW1-READ-CNT.
*
 CZKONWW1-READ-EXIT.
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
     IF       KNW-F18 NOT = SPACE
        IF    KNW-F18     = "D"
              PERFORM       CZKONTK3-DLT-SEC
        END-IF
        IF    KNW-F18     = "H"
              PERFORM       CZKONTK3-RWT-SEC
        END-IF
     END-IF.
*
 MAIN-999.
     PERFORM  CZKONWW1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*    検品結果梱包ファイル削除　　
****************************************************************
 CZKONTK3-DLT-SEC          SECTION.
*
     MOVE   "CZKONTK3-DLT-SEC"    TO   S-NAME.
*
 CZKONTK3-DLT-01.
     MOVE     SPACE               TO   KON-REC.
     INITIALIZE                        KON-REC.
*
     MOVE     KNW-F01             TO   KON-F01.
     MOVE     KNW-F02             TO   KON-F02.
     MOVE     KNW-F03             TO   KON-F03.
     MOVE     KNW-F04             TO   KON-F04.
     MOVE     KNW-F05             TO   KON-F05.
     MOVE     KNW-F06             TO   KON-F06.
     MOVE     KNW-F07             TO   KON-F07.
     MOVE     KNW-F08             TO   KON-F08.
     MOVE     KNW-F10             TO   KON-F10.
     MOVE     KNW-F11             TO   KON-F11.
     MOVE     KNW-F09             TO   KON-F09.
*
     PERFORM  CZKONTK3-READ-SEC.
     IF       CZKONTK3-INV-FLG = "INV"
              GO                    TO   CZKONTK3-DLT-02
     ELSE
              DELETE                     CZKONTK3
              ADD      1            TO   CZKONTK3-DLT-CNT
              GO                    TO   CZKONTK3-DLT-EXIT
     END-IF.
*
 CZKONTK3-DLT-02.
     DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS.
     DISPLAY NC"梱包ファイルＫＥＹなし！？"   UPON CONS.
     DISPLAY NC"　・バッチ日付　　＝" KNW-F01 UPON CONS.
     DISPLAY NC"　・バッチ時刻　　＝" KNW-F02 UPON CONS.
     DISPLAY NC"　・取引先ＣＤ　　＝" KNW-F03 UPON CONS.
     DISPLAY NC"　・倉庫ＣＤ　　　＝" KNW-F04 UPON CONS.
     DISPLAY NC"　・センター納品日＝" KNW-F05 UPON CONS.
     DISPLAY NC"　・センターＣＤ　＝" KNW-F06 UPON CONS.
     DISPLAY NC"　・店舗納品日　　＝" KNW-F07 UPON CONS.
     DISPLAY NC"　・店舗ＣＤ　　　＝" KNW-F08 UPON CONS.
     DISPLAY NC"　・伝票番号　　　＝" KNW-F10 UPON CONS.
     DISPLAY NC"　・行番号　　　　＝" KNW-F11 UPON CONS.
     DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS.
     GO                    TO   CZKONTK3-DLT-EXIT.
*
 CZKONTK3-DLT-EXIT.
     EXIT.
*
****************************************************************
*    検品結果梱包ファイル更新　　
****************************************************************
 CZKONTK3-RWT-SEC          SECTION.
*
     MOVE   "CZKONTK3-RWT-SEC"    TO   S-NAME.
*
 CZKONTK3-RWT-01.
     MOVE     SPACE               TO   KON-REC.
     INITIALIZE                        KON-REC.
*
     MOVE     KNW-F01             TO   KON-F01.
     MOVE     KNW-F02             TO   KON-F02.
     MOVE     KNW-F03             TO   KON-F03.
     MOVE     KNW-F04             TO   KON-F04.
     MOVE     KNW-F05             TO   KON-F05.
     MOVE     KNW-F06             TO   KON-F06.
     MOVE     KNW-F07             TO   KON-F07.
     MOVE     KNW-F08             TO   KON-F08.
     MOVE     KNW-F10             TO   KON-F10.
     MOVE     KNW-F11             TO   KON-F11.
     MOVE     KNW-F09             TO   KON-F09.
*
     PERFORM  CZKONTK3-READ-SEC.
     IF       CZKONTK3-INV-FLG = "INV"
              GO                    TO   CZKONTK3-RWT-02
     ELSE
              MOVE     KNW-F16      TO   KON-F16
              REWRITE                    KON-REC
              ADD      1            TO   CZKONTK3-RWT-CNT
              GO                    TO   CZKONTK3-RWT-EXIT
     END-IF.
*
 CZKONTK3-RWT-02.
     DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS.
     DISPLAY NC"梱包ファイルＫＥＹなし！？"   UPON CONS.
     DISPLAY NC"　・バッチ日付　　＝" KNW-F01 UPON CONS.
     DISPLAY NC"　・バッチ時刻　　＝" KNW-F02 UPON CONS.
     DISPLAY NC"　・取引先ＣＤ　　＝" KNW-F03 UPON CONS.
     DISPLAY NC"　・倉庫ＣＤ　　　＝" KNW-F04 UPON CONS.
     DISPLAY NC"　・センター納品日＝" KNW-F05 UPON CONS.
     DISPLAY NC"　・センターＣＤ　＝" KNW-F06 UPON CONS.
     DISPLAY NC"　・店舗納品日　　＝" KNW-F07 UPON CONS.
     DISPLAY NC"　・店舗ＣＤ　　　＝" KNW-F08 UPON CONS.
     DISPLAY NC"　・伝票番号　　　＝" KNW-F10 UPON CONS.
     DISPLAY NC"　・行番号　　　　＝" KNW-F11 UPON CONS.
     DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS.
     GO                    TO   CZKONTK3-RWT-EXIT.
*
 CZKONTK3-RWT-EXIT.
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
     READ     CZKONTK3
          INVALID
              MOVE     "INV"      TO   CZKONTK3-INV-FLG
              GO                  TO   CZKONTK3-READ-EXIT
          NOT INVALID
              MOVE     "   "      TO   CZKONTK3-INV-FLG
              GO                  TO   CZKONTK3-READ-EXIT
     END-READ.
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
*  ＝出荷エラー梱包データ読込件数
     DISPLAY "CZKONWW1 READ-CNT  = " CZKONWW1-READ-CNT UPON CONS.
*  ＝検品結果梱包ファイル削除件数
     DISPLAY "CZKONWWF DLT-CNT   = " CZKONTK3-DLT-CNT  UPON CONS.
*  ＝検品結果梱包ファイル更新件数
     DISPLAY "CZKONWWF RWT-CNT   = " CZKONTK3-RWT-CNT  UPON CONS.
*
     CLOSE     CZKONWW1
               CZKONTK3.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
