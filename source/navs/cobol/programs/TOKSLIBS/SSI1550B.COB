****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　セキチュー支払照合　植物　　　　　*
*    モジュール名　　　　：　照合済データ作成　　　　　　　　　*
*    作成日／更新日　　　：　10/04/22                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払データと請求合計ファイルを順で*
*                        ：　読み、伝票番号が合致した場合、照合*
*                            済データに明細情報を出力する。    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI1550B.
 AUTHOR.                OONO.
 DATE-WRITTEN.          10/04/22.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払データ >>--*
     SELECT   SEKSSIF  ASSIGN         DA-01-S-SEKSSIF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SSI-ST.
*----<< 請求合計データ >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SEIGKF21
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01  SEI-F05
                        STATUS         SEI-ST.
*----<< 照合済ファイル >>--*
     SELECT   SYOGOSEK  ASSIGN         DA-01-S-SYOGOSEK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SYO-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払データ >>--*
 FD  SEKSSIF           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 40   RECORDS.
     COPY     SEKSSIF   OF        XFDLIB
              JOINING   SSI       PREFIX.
*----<< 請求合計データ >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 照合済ファイル >>--*
 FD  SYOGOSEK           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 40   RECORDS.
     COPY     SEKSSIF   OF        XFDLIB
              JOINING   SYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  SYO-ST             PIC  X(02).
*----<< ﾌﾟﾛｸﾞﾗﾑID  >>--*
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "SSI1550B".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*----<< READ KEY >>--*
 01  SSI-KEY.
     03  SSI-DENNO      PIC  X(09).
 01  SEI-KEY.
     03  SEI-DENNO      PIC  X(09).
*----<< 金額データ変換 >>--*
 01  WK-SIHARAI         PIC  X(09).
 01  WK-SIHARAI-R       REDEFINES     WK-SIHARAI.
     03  HEN-SIHARAI    PIC  9(09).
 01  WK-KINGAKU         PIC S9(09).
 01  WK-SEIKYU          PIC  X(10)   VALUE  ZERO.
*----<< 取引先変換 >>--*
 01  WK-TORICD          PIC  X(06).
 01  WK-TORICD-R        REDEFINES     WK-TORICD.
     03  HEN-TORICD     PIC  9(06).
 01  WK-TORIHIKI        PIC  9(06).
*----<< ｴﾗｰｶｳﾝﾄ >>--*
 01  SEI-ERR            PIC  9(05)    VALUE   ZERO.
 01  SIH-ERR            PIC  9(05)    VALUE   ZERO.
 01  KIN-ERR            PIC  9(05)    VALUE   ZERO.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払データ >>--*
 SSI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEKSSIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " SEKSSIF ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計ファイル >>--*
 SEI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " HSEIGKF ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 照合ファイル >>--*
 SYO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYOGOSEK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " SYOGOSEK ERROR " SYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*開始メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
******
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     SSI-KEY  =  HIGH-VALUE
                                  AND  SEI-KEY  =  HIGH-VALUE.
     PERFORM  300-END-RTN.
******
*終了メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     SEKSSIF.
     OPEN     INPUT     HSEIGKF.
     OPEN     OUTPUT    SYOGOSEK.
*キーの初期化
     MOVE     LOW-VALUE      TO   SSI-KEY   SEI-KEY.
*ファイル初期ＲＥＡＤ（キーのセット）
     PERFORM  900-SSI-READ.
     MOVE     926061         TO   WK-TORICD.
     MOVE     HEN-TORICD     TO   WK-TORIHIKI.
     MOVE     SPACE          TO   WK-TORICD.
     PERFORM  900-SEI-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     EVALUATE  TRUE
*請求データなしチェック
         WHEN      SSI-KEY   <    SEI-KEY
               PERFORM  900-SSI-READ
*正常データ（金額アンマッチチェック）
         WHEN      SSI-KEY   =    SEI-KEY
               PERFORM  220-HENSYU-RTN
               PERFORM  900-SSI-READ
               PERFORM  900-SEI-READ
*支払情報データなしチェック
         WHEN      SSI-KEY   >    SEI-KEY
               PERFORM  900-SEI-READ
     END-EVALUATE.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    SEKSSIF.
     CLOSE    HSEIGKF.
     CLOSE    SYOGOSEK.
*エラー件数表示
**** DISPLAY "ｾｲｷｭｳ ﾃﾞｰﾀﾅｼ = " SEI-ERR UPON CONS.
**** DISPLAY "ｼﾊﾗｲ  ﾃﾞｰﾀﾅｼ = " SIH-ERR UPON CONS.
**** DISPLAY "ｷﾝｶﾞｸ ﾁｶﾞｲ   = " KIN-ERR UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     照合済ファイル出力（金額チェック）           *
*--------------------------------------------------------------*
 220-HENSYU-RTN         SECTION.
*レコード転送
     MOVE     SSI-REC             TO   SYO-REC.
*金額比較（レコード区分＝””→正常データ）
*        （レコード区分＝”２”→支払金額違い）
     IF       SSI-F10        =    SEI-F06
              CONTINUE
     ELSE
              GO                  TO   220-HENSYU-RTN-EXIT
     END-IF.
*照合ファイル出力
     WRITE    SYO-REC.
*
 220-HENSYU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払データ　　　　 READ                      *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
     READ     SEKSSIF
         AT END
              MOVE      HIGH-VALUE     TO   SSI-KEY
         NOT AT END
              MOVE      SSI-F04        TO   SSI-DENNO
     END-READ.
 900-SSI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 START                     *
*--------------------------------------------------------------*
 900-SEI-ST-READ        SECTION.
*
     MOVE     926061         TO      SEI-F01.
     MOVE     SSI-F10        TO      SEI-F05.
     START    HSEIGKF   KEY  IS  >=  SEI-F01  SEI-F05
              INVALID
              MOVE      HIGH-VALUE   TO       SEI-KEY
     END-START.
*
 900-SEI-ST-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
*
     IF       SEI-KEY  =  HIGH-VALUE
              GO          TO           900-SEI-READ-EXIT
     END-IF.
     READ     HSEIGKF
         AT END
              MOVE      HIGH-VALUE     TO   SEI-KEY
         NOT AT END
              MOVE      SEI-F05        TO   SEI-DENNO
     END-READ.
     IF       WK-TORIHIKI NOT =  SEI-F01
              MOVE      HIGH-VALUE     TO   SEI-KEY
     END-IF.
*
 900-SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
