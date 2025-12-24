# SOKOPRT

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/SOKOPRT.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *    倉庫側　スプール　変更　　　　　　　　　　　         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN    :=       @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)
                                      /.##片岡配送センター##./
    IF         ?WKSTN    = 'WKSTN201'
        THEN   REFSPL @OUT,XOUTQ5
    END
                                      /.##片岡配送センター##./
    IF         ?WKSTN    = 'WKSTNH6A'
        THEN   REFSPL @OUT,XXPKATQ
    END
                                      /.##大和倉庫##./
    IF         ?WKSTN    = 'WKSTN212'
        THEN   REFSPL @OUT,XOUTQ6
    END
                                      /.##大和倉庫（ＰＣ）##./
    IF         ?WKSTN    = 'WKSTNH60'
        THEN   REFSPL @OUT,XXPDIWQ
    END
                                      /.##フバサミクレー##./
    IF         ?WKSTN    = 'WKSTN203'
        THEN   REFSPL @OUT,XOUTQ7
    END
                                      /.##フバサミクレー##./
    IF         ?WKSTN    = 'WKSTNH63'
        THEN   REFSPL @OUT,XXPFUBQ
    END
                                      /.##フバサミクレー３##./
    IF         ?WKSTN    = 'WKSTNH6F'
        THEN   REFSPL @OUT,XXPFUB2Q
    END
                                      /.##本社（大型プリンタ）##./
    IF         ?WKSTN    = 'WKSTN001'
        THEN   REFSPL @OUT,XSYSLSTQ
    END
                                      /.##富岡配送センター##./
    IF         ?WKSTN    = 'WKSTN204'
        THEN   REFSPL @OUT,XOUTQ4
    END
                                      /.##富岡配送センター(PC)##./
    IF         ?WKSTN    = 'WKSTNH83'
        THEN   REFSPL @OUT,XXPTOMQ
    END
                                      /.##鴻巣配送センター##./
    IF         ?WKSTN    = 'WKSTN215'
        THEN   REFSPL @OUT,XXKOUQ
    END
                                      /.##鴻巣配送センター(PC)##./
    IF         ?WKSTN    = 'WKSTNH84'
        THEN   REFSPL @OUT,XXPKOUQ
    END
                                      /.##西尾植物_(K1300)##./
    IF         ?WKSTN    = 'WKSTN216'
        THEN   REFSPL @OUT,XXNISQ2
    END
                                      /.##西尾植物_(PC)   ##./
    IF         ?WKSTN    = 'WKSTNH86'
        THEN   REFSPL @OUT,XXPNISQ2
    END
                                      /.##西尾植物_(PC)   ##./
    IF         ?WKSTN    = 'WKSTNH8A'
        THEN   REFSPL @OUT,XXPNISQ2
    END
                                      /.##西尾植物_(K1500)##./
    IF         ?WKSTN    = 'WKSTN226'
        THEN   REFSPL @OUT,XXNISQ1
    END
                                      /.##手綱園芸##./
    IF         ?WKSTN    = 'WKSTN207'
        THEN   REFSPL @OUT,XXTEZQ
    END
                                      /.##手綱園芸##./
    IF         ?WKSTN    = 'WKSTN217'
        THEN   REFSPL @OUT,XXTEZQ
    END
                                      /.##手綱園芸(PC)##./
    IF         ?WKSTN    = 'WKSTNH90'
        THEN   REFSPL @OUT,XXPTEZQ
    END
                                      /.##北上配送(PC)##./
    IF         ?WKSTN    = 'WKSTNHE2'
        THEN   REFSPL @OUT,XXPHITQ
    END
                                      /.##北上配送(PC)##./
    IF         ?WKSTN    = 'WKSTNHE2'
        THEN   REFSPL @OUT,XXPHITQ
    END
                                      /.##仙台営業所##./
    IF         ?WKSTN    = 'WKSTNH42'
        THEN   REFSPL @OUT,XXPSENQ
    END
                                      /.##蔦井倉庫(PC)##./
    IF         ?WKSTN    = 'WKSTNHT9'
        THEN   REFSPL @OUT,XXPTUTQ
    END
                                      /.##北海道支店##./
    IF         ?WKSTN    = 'WKSTNH45'
        THEN   REFSPL @OUT,XXPHOKQ
    END
                                      /.##カトーレック岡山##./
    IF         ?WKSTN    = 'WKSTNHT5'
        THEN   REFSPL @OUT,XXPKARQ
    END
                                      /.##花の海##./
    IF         ?WKSTN    = 'WKSTNHT4'
        THEN   REFSPL @OUT,XXPHANQ
    END
                                      /.##長野セルトップ##./
    IF         ?WKSTN    = 'WKSTNH22'
        THEN   REFSPL @OUT,XXPNAGQ
    END
                                      /.##秋田園芸資材##./
    IF         ?WKSTN    = 'WKSTNH6S'
        THEN   REFSPL @OUT,XXPAKIQ
    END
                                      /.##岡山総合花卉##./
    IF         ?WKSTN    = 'WKSTNH53'
        THEN   REFSPL @OUT,XXPOKSQ
    END
                                      /.##ナブ・アシスト##./
    IF         ?WKSTN    = 'NAVWS   '
        THEN   REFSPL @OUT,XXNAVQ
    END
                                      /.##片岡配送追加##./
    IF         ?WKSTN    = 'WKSTNH6B'
        THEN   REFSPL @OUT,XXPKATQ
    END
                                      /.##さくら配送１##./
    IF         ?WKSTN    = 'WKSTNHS1'
        THEN   REFSPL @OUT,XXPSKRQ1
    END
                                      /.##さくら配送２##./
    IF         ?WKSTN    = 'WKSTNHS2'
        THEN   REFSPL @OUT,XXPSKRQ2
    END
                                      /.##新札幌配送　##./
    IF         ?WKSTN    = 'WKSTNHTU'
        THEN   REFSPL @OUT,XXPAWSQ
    END
                                      /.##カトーレック岡山##./
    IF         ?WKSTN    = 'WKSTNHKR'
        THEN   REFSPL @OUT,XXPKARQP
    END
                                      /.##ＦＬＫ九州##./
    IF         ?WKSTN    = 'WKSTNHFL'
        THEN   REFSPL @OUT,XXPFLKQL
    END

    RETURN    PGMEC-@PGMEC

```
