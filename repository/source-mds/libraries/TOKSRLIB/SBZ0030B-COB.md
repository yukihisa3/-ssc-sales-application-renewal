# SBZ0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBZ0030B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　部門間在庫入出庫ＤＴ作成　　　　  *
*    作成日／更新日　　　：　2018/01/19                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　部門間移動集計Ｆを読み、振替元部　*
*                        ：　門、振替先部門、倉庫、振替日毎又　*
*                        ：　は６行毎に振替移動実績Ｆを作成する*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBZ0030B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 部門間移動集計ファイル >>******************************
     SELECT   BUMSYUF   ASSIGN    TO        DA-01-VI-BUMSYUL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SYU-F95 SYU-F01
                                                 SYU-F02 SYU-F03
                                                 SYU-F04 SYU-F05
                                                 SYU-F06 SYU-F07
                                                 SYU-F08
                        FILE      STATUS    IS   SYU-STATUS.
****<< 振替移動実績ファイル >>********************************
     SELECT   FURIDOF   ASSIGN    TO        DA-01-VI-FURIDOL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   IDO-F01 IDO-F02
                                                 IDO-F07
                        FILE      STATUS    IS   IDO-STATUS.
****<< 条件ファイル >>****************************************
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 部門間移動集計ファイル >>******************************
 FD  BUMSYUF.
     COPY     BUMSYUF   OF        XFDLIB
              JOINING   SYU       PREFIX.
****<< 振替移動実績ファイル >>********************************
 FD  FURIDOF.
     COPY     FURIDOF   OF        XFDLIB
              JOINING   IDO       PREFIX.
****<< 条件ファイル >>****************************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 SYU-STATUS           PIC  X(02).
     02 IDO-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBZ0030B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  JYO-INV-FLG             PIC  X(03)  VALUE  SPACE.
 01  WK-GYONO                PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  ERR-CNT             PIC  9(07)   VALUE  0.
     03  CRT-CNT             PIC  9(07)   VALUE  0.
*キー退避
 01  WK-KEY.
     03  WK-SYU-F01          PIC  X(04)   VALUE  SPACE.
     03  WK-SYU-F02          PIC  X(02)   VALUE  SPACE.
     03  WK-SYU-F03          PIC  X(04)   VALUE  SPACE.
     03  WK-SYU-F04          PIC  9(08)   VALUE  ZERO.
     03  WK-SYU-F05          PIC  X(08)   VALUE  SPACE.
     03  WK-SYU-F06          PIC  X(05)   VALUE  SPACE.
     03  WK-SYU-F07          PIC  X(02)   VALUE  SPACE.
     03  WK-SYU-F08          PIC  X(01)   VALUE  SPACE.
**条件ファイルＲＥＣ保管
 COPY     JYOKEN1            OF  XFDLIB
 JOINING  JYO-KEIRI-SIME     AS  PREFIX.
 COPY     JYOKEN1            OF  XFDLIB
 JOINING  JYO-KEIRI-TUKI     AS  PREFIX.
 COPY     JYOKEN1            OF  XFDLIB
 JOINING  JYO-DENNO          AS  PREFIX.
*
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
*
 01  SYS-DATE8               PIC  9(08)  VALUE  ZERO.
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
 01  WK-SAGYOUBI-1           PIC  9(08)  VALUE  ZERO.
 01  WK-SAGYOUBI-1-YMD       REDEFINES   WK-SAGYOUBI-1.
     03  WK-SAGYOUBI-Y       PIC  9(04).
     03  WK-SAGYOUBI-M       PIC  9(02).
     03  WK-SAGYOUBI-D       PIC  9(02).
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*経理〆日
 01  WK-SIME                 PIC  9(08).
*経理〆月初日
 01  WK-SIME01               PIC  9(08).
 01  WK-SIME01X              REDEFINES WK-SIME01.
     03 WK-SIME01-YYYY       PIC  9(04).
     03 WK-SIME01-MM         PIC  9(02).
     03 WK-SIME01-DD         PIC  9(02).
 01  WK-SIME01-MM-WK         PIC  9(09)V99.
*経理〆月末日
 01  WK-SIME99               PIC  9(08).
 01  WK-SIME99X              REDEFINES WK-SIME99.
     03 WK-SIME99-YYYY       PIC  9(04).
     03 WK-SIME99-MM         PIC  9(02).
     03 WK-SIME99-DD         PIC  9(02).
*システム日付の月初日取得
 01  WK-SYSDT-ST.
     03  WK-SYSDT-YYYY       PIC  9(04)  VALUE  ZERO.
     03  WK-SYSDT-MM         PIC  9(02)  VALUE  ZERO.
     03  WK-SYSDT-DD         PIC  9(02)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-BUMON         PIC  X(04).
 01  PARA-TANCD         PIC  X(02).
************************************************************
 PROCEDURE              DIVISION USING  PARA-BUMON  PARA-TANCD.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  BUMSYUF.
     MOVE   "BUMSYUL2"        TO    ERR-FL-ID.
     MOVE    SYU-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  FURIDOF.
     MOVE   "FURIDOL1"        TO    ERR-FL-ID.
     MOVE    IDO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "JYOKEN1 "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         I-O       BUMSYUF.
     OPEN         I-O       FURIDOF.
     OPEN         INPUT     HJYOKEN.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   WK-DATE8
                                       SYS-DATE8
                                       WK-SYSDT-ST.
     DISPLAY "# HIDUKE = " WK-DATE8   UPON CONS.
     DISPLAY "# JIKAN  = " WK-TIME-HM UPON CONS.
*条件ファイルＲＥＣ事前取得（経理締め月度）
     MOVE     "58"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY
                NC"＃＃条件ファイル取得エラー（経理締月度）＃＃"
                                                       UPON CONS
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
     MOVE     JYO-REC        TO   JYO-KEIRI-SIME-REC.
*条件ファイルＲＥＣ事前取得（月別経理締め年月日）
     MOVE     "99"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY
                NC"＃＃条件ファイル取得エラー（経理締め日）＃＃"
                                                       UPON CONS
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
     MOVE     JYO-REC        TO   JYO-KEIRI-TUKI-REC.
*
*経理月の１日を求める
     MOVE     WK-DATE8            TO   WK-SIME01.
     MOVE     JYO-KEIRI-SIME-F04  TO   WK-SIME01-MM-WK.
     MOVE     WK-SIME01-MM-WK     TO   WK-SIME01-MM.
     IF     ( WK-DATE8(5:2) = 01  ) AND
            ( WK-SIME01-MM =  12  )
              COMPUTE WK-SIME01-YYYY = WK-SIME01-YYYY - 1
     END-IF.
     MOVE     01                  TO   WK-SIME01-DD.
     DISPLAY NC"経理月１日＝" WK-SIME01 UPON CONS.
*発注日・納品日のチェック対象となる締処理年月日を特定する
     EVALUATE   JYO-KEIRI-SIME-F04
         WHEN   01    MOVE  JYO-KEIRI-TUKI-F04   TO   WK-SIME
         WHEN   02    MOVE  JYO-KEIRI-TUKI-F05   TO   WK-SIME
         WHEN   03    MOVE  JYO-KEIRI-TUKI-F06   TO   WK-SIME
         WHEN   04    MOVE  JYO-KEIRI-TUKI-F07   TO   WK-SIME
         WHEN   05    MOVE  JYO-KEIRI-TUKI-F08   TO   WK-SIME
         WHEN   06    MOVE  JYO-KEIRI-TUKI-F09   TO   WK-SIME
         WHEN   07    MOVE  JYO-KEIRI-TUKI-F10   TO   WK-SIME
         WHEN   08    MOVE  JYO-KEIRI-TUKI-F11   TO   WK-SIME
         WHEN   09    MOVE  JYO-KEIRI-TUKI-F12   TO   WK-SIME
         WHEN   10    MOVE  JYO-KEIRI-TUKI-F12A  TO   WK-SIME
         WHEN   11    MOVE  JYO-KEIRI-TUKI-F12B  TO   WK-SIME
         WHEN   12    MOVE  JYO-KEIRI-TUKI-F12C  TO   WK-SIME
     END-EVALUATE.
*システム日付の翌月末日を求める
     MOVE     SYS-DATE8      TO   WK-SIME99.
     ADD      100            TO   WK-SIME99.
     IF       WK-SIME99-MM   =    13
              MOVE   01      TO   WK-SIME99-MM
              ADD    10000   TO   WK-SIME99
     END-IF.
     MOVE     99             TO   WK-SIME99-DD.
     DISPLAY NC"翌月末日　＝" WK-SIME99 UPON CONS.
*経理締日の１日前を求める
     DISPLAY NC"経理〆日　＝" WK-SIME UPON CONS.
*今回は締日からマイナス１しない
*****COMPUTE  WK-SIME  =  WK-SIME  -  1.
*****DISPLAY NC"締日１日前＝" WK-SIME UPON CONS.
*#2016/11/14 NAV ST システム日付の月初日を算出
     MOVE    1               TO   WK-SYSDT-DD.
     DISPLAY NC"システム日付の月初日＝" WK-SYSDT-ST UPON CONS.
*
*条件ファイルＲＥＣ事前取得（伝票番号採番値）
     MOVE     35             TO   JYO-F01.
     MOVE     "FURIKAE "     TO   JYO-F02.
     PERFORM  JYO-READ-SEC.
     IF       JYO-INV-FLG    =    "INV"
              DISPLAY
              NC"＃＃条件ファイル取得エラー（伝番採番値）＃＃"
                                                       UPON CONS
              MOVE  4001     TO   PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
     MOVE     JYO-REC        TO   JYO-DENNO-REC.
*ここで一旦条件Ｆをクローズする。
     CLOSE   HJYOKEN.
*部門間移動集計ファイル読込
     PERFORM  BUMSYUF-READ-SEC.
*
     IF  END-FLG  =  "END"
         DISPLAY NC"＃＃処理対象データがありません！＃＃"
         UPON CONS
     END-IF.
*
 100-INIT-END.
     EXIT.
****************************************************************
*    条件ファイル索引
****************************************************************
 JYO-READ-SEC               SECTION.
*
     READ     HJYOKEN
       INVALID
              MOVE "INV"     TO   JYO-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   JYO-INV-FLG
     END-READ.
 JYO-READ-EXIT.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*ブレイクチェック／行数チェック
     IF  WK-SYU-F01  =  SYU-F01
     AND WK-SYU-F02  =  SYU-F02
     AND WK-SYU-F03  =  SYU-F03
     AND WK-SYU-F04  =  SYU-F04
     AND WK-GYONO    <  6
         CONTINUE
     ELSE
*********伝票番号を採番
         ADD     1               TO   JYO-DENNO-F04
         IF   JYO-DENNO-F04  >  JYO-DENNO-F06
              MOVE JYO-DENNO-F05 TO   JYO-DENNO-F04
         END-IF
         MOVE    ZERO            TO   WK-GYONO
         MOVE    SYU-F01         TO   WK-SYU-F01
         MOVE    SYU-F02         TO   WK-SYU-F02
         MOVE    SYU-F03         TO   WK-SYU-F03
         MOVE    SYU-F04         TO   WK-SYU-F04
     END-IF.
*振替移動実績ファイル作成
     PERFORM  FURIDOF-WT-SEC.
*
     PERFORM BUMSYUF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    部門間移動集計ファイル読込
************************************************************
 BUMSYUF-READ-SEC                   SECTION.
*
     READ   BUMSYUF
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        BUMSYUF-READ-EXIT
     END-READ.
*件数カウント
     ADD     1         TO        IN-CNT.
*振替移動ＤＴ作成ＦＬＧ＝１になったら、ＰＧを終了する。
     IF  SYU-F95  =  "1"
         MOVE          "END"     TO   END-FLG
         GO            TO        BUMSYUF-READ-EXIT
     END-IF.
*
 BUMSYUF-READ-EXIT.
     EXIT.
************************************************************
*    振替移動実績ファイル作成
************************************************************
 FURIDOF-WT-SEC                       SECTION.
*レコード初期化
     MOVE        SPACE           TO   IDO-REC.
     INITIALIZE                       IDO-REC.
*行番号カウントアップ
     ADD         1               TO   WK-GYONO.
*伝票番号
     MOVE        JYO-DENNO-F04   TO   IDO-F01.
*行番号
     MOVE        WK-GYONO        TO   IDO-F02.
*振替先部門
     MOVE        SYU-F01         TO   IDO-F03.
*振替先場所
     MOVE        SYU-F02         TO   IDO-F04.
*振替元部門
     MOVE        SYU-F03         TO   IDO-F05.
*振替先場所
     MOVE        SYU-F02         TO   IDO-F06.
*振替日
     MOVE        SYU-F04         TO   IDO-F07.
*サカタ商品ＣＤ
     MOVE        SYU-F05         TO   IDO-F08.
*品単１
     MOVE        SYU-F06         TO   IDO-F09.
*品単２
     MOVE        SYU-F07         TO   IDO-F10.
*品単３
     MOVE        SYU-F08         TO   IDO-F11.
*数量
     MOVE        SYU-F09         TO   IDO-F12.
*登録日付
     MOVE        WK-DATE8        TO   IDO-F92.
*登録時刻
     MOVE        WK-TIME-HM      TO   IDO-F93.
*登録担当者部門ＣＤ
     MOVE        PARA-BUMON      TO   IDO-F94.
*登録担当者ＣＤ
     MOVE        PARA-TANCD      TO   IDO-F95.
*振替日チェック
* _「経理月」の１日≦振替日≦システム日付の翌月末
     IF ( WK-SIME01 <= SYU-F04   ) AND
        ( SYU-F04   <= WK-SIME99 )
         CONTINUE
     ELSE
         MOVE    "1"      TO      IDO-F83
     END-IF.
* _「システム日付」の１日≦振替日≦システム日付の翌月末
     IF ( WK-SIME < WK-DATE8  )
         IF ( WK-SYSDT-ST <= SYU-F04 ) AND
            ( SYU-F04   <= WK-SIME99 )
              CONTINUE
         ELSE
              MOVE    "1"      TO      IDO-F83
         END-IF
     END-IF.
*振替日エラーチェック
     IF  IDO-F83 NOT = SPACE
         ADD           1       TO      ERR-CNT
     END-IF.
*振替移動実績ファイル更新
     WRITE  IDO-REC.
     ADD               1       TO      CRT-CNT.
*振替移動集計ファイル更新
     MOVE   IDO-F01            TO      SYU-F93.
     MOVE   IDO-F02            TO      SYU-F94.
     MOVE   "1"                TO      SYU-F95.
     MOVE   WK-DATE8           TO      SYU-F96.
     MOVE   WK-TIME-HM         TO      SYU-F97.
     REWRITE  SYU-REC.
*
 FURIDOF-WT-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE             BUMSYUF  FURIDOF.
*振替移動実績Ｆ作成が行われたとき→伝票番号を更新する。
     IF  CRT-CNT  >  ZERO
         OPEN I-O  HJYOKEN
*********条件ファイル伝票番号採番値更新
         MOVE     35             TO   JYO-F01
         MOVE     "FURIKAE "     TO   JYO-F02
         PERFORM  JYO-READ-SEC
         IF       JYO-INV-FLG    =    "INV"
                  MOVE  SPACE    TO   JYO-REC
                  INITIALIZE          JYO-REC
                  MOVE  JYO-DENNO-REC  TO  JYO-REC
                  WRITE JYO-REC
         ELSE
                  MOVE  JYO-DENNO-REC  TO  JYO-REC
                  REWRITE JYO-REC
         END-IF
         CLOSE     HJYOKEN
     END-IF.
*
     DISPLAY "* BUMSYUF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* FURIKAEBI ERR    = " ERR-CNT  " *"  UPON CONS.
     DISPLAY "* FURIDOF (CRT   ) = " CRT-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
